#![feature(result_option_inspect)]

use cli::CmdInput;
use std::str::FromStr;
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::Flags;
use cranelift_codegen::Context;
use cranelift_entity::{SecondaryMap, EntitySet};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use gen::*;
use instance::*;
use lexer::*;
use modules::*;
use parser::*;
use typec::*;
use std::process::Command;
use std::{collections::VecDeque, path::Path};

macro_rules! unwrap {
    ($expr:expr, $err:ident, $mapping:expr) => {
        match ($expr) {
            Ok(p) => p,
            Err($err) => {
                return Err($mapping);
            }
        }
    };
}

/// compiles the catalyst source code, taking command like args as input,
/// complete compilation only works on windows with msvc and you have to invoke
/// compiler inside Native Tools Command Prompt
pub fn compile() -> std::result::Result<(), Box<dyn std::fmt::Display>> {
    // cli
    let input = CmdInput::new();

    assert!(&input.args()[0] == "c");

    let path = Path::new(&input.args()[1]);

    // lexer
    let mut sources = Sources::new();
    let mut builtin_source = BuiltinSource::new(&mut sources);

    // parser
    let mut ast = ast::Data::new();
    let mut ast_temp = ast::Temp::new();

    // modules
    let mut scope = Scope::new();
    let mut module_map = Map::new();
    let mut unit_load_ctx = LoaderContext::new();
    let mut modules = Modules::new();
    let mut units = Units::new();
    
    let scope_item_lexicon = {
        let mut map = ScopeItemLexicon::new();

        for (id, name) in [
            (std::any::TypeId::of::<module::Module>(), "module"),
            (std::any::TypeId::of::<Ty>(), "type"),
            (std::any::TypeId::of::<Func>(), "function"),
            (std::any::TypeId::of::<Tir>(), "tir"),
        ] {
            map.insert(id, name);
        }

        map
    };
    
    let module_order = {
        let mut module_frontier = VecDeque::new();
        
        let unit_order = unwrap!(
            unit::Loader {
                sources: &mut sources,
                units: &mut units,
                ctx: &mut unit_load_ctx,
            }
            .load_units(path),
            err,
            Box::new(modules::error::Display::new(
                sources,
                err,
                modules,
                units,
                scope_item_lexicon
            ))
        );

        let mut module_order = vec![];

        for unit in unit_order {
            let local_module_order = unwrap!(
                module::Loader {
                    sources: &mut sources,
                    modules: &mut modules,
                    units: &mut units,
                    frontier: &mut module_frontier,
                    ctx: &mut unit_load_ctx,
                    map: &mut module_map,
                }
                .load_unit_modules(unit),
                err,
                Box::new(modules::error::Display::new(
                    sources,
                    err,
                    modules,
                    units,
                    scope_item_lexicon
                ))
            );

            module_order.extend(local_module_order.into_iter().rev());
        }

        module_order.reverse();

        module_order
    };

    let isa = {
        let setting_builder = cranelift_codegen::settings::builder();
        let flags = Flags::new(setting_builder);
    
        let target_triple = input
            .field("target")
            .map_or_else(
                || target_lexicon::Triple::host(), 
                |target| target_lexicon::triple!(target),     
            );
        cranelift_codegen::isa::lookup(target_triple)
            .unwrap()
            .finish(flags)
            .unwrap()
    };

    // typec
    let mut t_types = typec::Types::new();
    let mut t_funcs = typec::Funcs::new();
    let mut t_graph = GenericGraph::new();
    
    let mut bodies = SecondaryMap::new();
    /* perform type checking and build tir */ {
        let builtin_items: Vec<module::Item> = {
            let builtin_types = [
                ("nothing", ty::Kind::Nothing),
                ("bool", ty::Kind::Bool),
                ("int", ty::Kind::Int(-1)),
                ("i8", ty::Kind::Int(8)),
                ("i16", ty::Kind::Int(16)),
                ("i32", ty::Kind::Int(32)),
                ("i64", ty::Kind::Int(64)),
            ];

            let mut vec = vec![];

            for (name, kind) in builtin_types {
                let span = builtin_source.make_span(&mut sources, name);
                let id = name.into();

                let ty = {
                    let ent = ty::Ent::new(kind, id);
                    t_graph.close_node();
                    t_types.ents.push(ent)
                };

                let item = module::Item::new(id, ty, span);
                vec.push(item);

                for name in "+ - * / < > <= >= == !=".split(' ') {
                    match kind {
                        ty::Kind::Bool if !"".contains(name) => {
                            continue;
                        }
                        ty::Kind::Int(_) if !"+ - * / < > <= >= == !=".contains(name) => {
                            continue;
                        }
                        _ => {}
                    }

                    let span = builtin_source.make_span(&mut sources, name);
                    let sig = {
                        let args = t_types.cons.list(&[ty, ty]);
                        let ret = if "< > <= >= == !=".contains(name) {
                            Ty(1).into() // bool
                        } else {
                            ty.into()
                        };
                        typec::Signature {
                            args,
                            ret,
                            call_conv: Span::default(),
                        }
                    };

                    let id = {
                        let name = sources.display(span);
                        typec::func::Builder::binary_id(id, ID::new(name))
                    };

                    let func = {
                        let ent = typec::func::Ent {
                            sig,
                            name: span,
                            id,
                            kind: typec::func::Kind::Builtin,
                            ..Default::default()
                        };
                        t_funcs.push(ent)
                    };

                    let item = module::Item::new(id, func, span);
                    vec.push(item);
                }
            }

            vec
        };

        let mut type_ast = SecondaryMap::new();
        let mut func_ast = SecondaryMap::new();
        let mut t_temp = tir::Temp::new();
    
        for module in module_order {
            let source = modules[module].source;
            let content = &sources[source].content;
    
            for item in builtin_items.iter() {
                scope.insert(source, item.id, item.to_scope_item()).unwrap();
            }
    
            ast.clear();
            let inter_state = unwrap!(
                Parser::parse_imports(content, &mut ast, &mut ast_temp, source),
                err,
                Box::new(parser::error::Display::new(sources, err))
            );
    
            if let Some(imports) = ModuleImports::new(&ast, &sources).imports() {
                for import in imports {
                    let name = sources.display(import.name);
                    let &dep = module_map.get((name, module)).unwrap();
                    for item in modules[dep].items.iter() {
                        scope.insert(source, item.id, item.to_scope_item()).unwrap();
                    }
                }
            }
    
            ast.clear();
            unwrap!(
                Parser::parse_code_chunk(content, &mut ast, &mut ast_temp, inter_state),
                err,
                Box::new(parser::error::Display::new(sources, err))
            );
    
            println!("{}", ast::FileDisplay::new(&ast, content));
    
            unwrap!(
                Collector {
                    nothing: Ty(0),
                    scope: &mut scope,
                    functions: &mut t_funcs,
                    types: &mut t_types,
                    modules: &mut modules,
                    func_ast: &mut func_ast,
                    sources: &sources,
                    ast: &ast,
                    type_ast: &mut type_ast,
                    module,
                }
                .collect_items(),
                err,
                Box::new(typec::error::Display::new(
                    sources,
                    err,
                    modules,
                    units,
                    t_types,
                    scope_item_lexicon
                ))
            );
    
            for ty in modules[module]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Ty>())
            {
                unwrap!(
                    typec::ty::Builder {
                        scope: &mut scope,
                        types: &mut t_types,
                        sources: &sources,
                        ast: &ast,
                        type_ast: &type_ast,
                        graph: &mut t_graph,
                        ty
                    }
                    .build(),
                    err,
                    Box::new(typec::error::Display::new(
                        sources,
                        err,
                        modules,
                        units,
                        t_types,
                        scope_item_lexicon
                    ))
                );
            }
    
            for func in modules[module]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Func>())
            {
                unwrap!(
                    typec::func::Builder {
                        nothing: Ty(0),
                        bool: Ty(1),
                        scope: &mut scope,
                        types: &mut t_types,
                        sources: &sources,
                        ast: &ast,
                        func,
                        funcs: &mut t_funcs,
                        func_ast: &func_ast,
                        body: &mut bodies[func],
                        temp: &mut t_temp,
                    }
                    .build(),
                    err,
                    Box::new(typec::error::Display::new(
                        sources,
                        err,
                        modules,
                        units,
                        t_types,
                        scope_item_lexicon
                    ))
                );
    
                println!(
                    "{}",
                    typec::tir::Display::new(&t_types, &sources, &bodies[func], t_funcs[func].body)
                );
            }
    
            scope.clear();
        }
    }

    // cranelift
    let mut func_builder_ctx = FunctionBuilderContext::new();
    let mut ctx = Context::new();

    // module
    let builder = ObjectBuilder::new(
        isa,
        "catalyst",
        cranelift_module::default_libcall_names(),
    )
    .unwrap();
    let mut module = ObjectModule::new(builder);

    // instance
    let mut function = instance::func::Func::new();
    let mut types = instance::types::Types::new();
    let ptr_ty = module.isa().pointer_type();
    let system_call_convention = module.isa().default_call_conv();

    TypeTranslator {
        t_types: &t_types,
        t_graph: &t_graph,
        types: &mut types,
        ptr_ty,
    }
    .translate()
    .unwrap();
    
    let mut func_lookup = SecondaryMap::new();
    let mut has_struct_ret = EntitySet::new();
    
    /* declare function headers */ {
        for (id, ent) in t_funcs.iter() {
            let name = sources.display(ent.name);
            let linkage = match ent.kind {
                typec::func::Kind::Local => Linkage::Export,
                typec::func::Kind::Builtin => continue,
                typec::func::Kind::External => Linkage::Import,
            };
    
            let returns_struct = instance::func::Translator::translate_signature(
                &ent.sig,
                &mut ctx.func.signature,
                &types,
                &t_types,
                system_call_convention,
            )
            .unwrap();
    
            if returns_struct {
                has_struct_ret.insert(id);
            }
    
            let func = module
                .declare_function(name, linkage, &ctx.func.signature)
                .unwrap();
    
            ctx.func.signature.clear(CallConv::Fast);
    
            func_lookup[id] = func.into();
        }
    }

    
    /* define */ {
        let mut variable_set = EntitySet::new();
        let mut stack_slot_lookup = SecondaryMap::new();
        let mut mir_to_ir_lookup = SecondaryMap::new();
        let mut ir_block_lookup = SecondaryMap::new();
        let mut tir_mapping = SecondaryMap::new();   
        
        for (func, ent) in t_funcs.iter() {
            if ent.kind != typec::func::Kind::Local {
                continue;
            }
    
            tir_mapping.clear();
            function.clear();
            instance::func::Translator {
                system_call_convention,
                func_id: func,
                types: &types,
                t_types: &t_types,
                t_funcs: &t_funcs,
                func: &mut function,
                tir_mapping: &mut tir_mapping,
                body: &bodies[func],
                nothing: Ty(0),
                ptr_ty,
                return_dest: None,
                has_struct_ret: &has_struct_ret,
                sources: &sources,
            }
            .translate_func()
            .unwrap();
    
            println!("{}", mir::Display::new(&sources, &function, &t_types));
    
            ctx.clear();
            mir_to_ir_lookup.clear();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);
            Generator {
                module: &mut module,
                function_lookup: &mut func_lookup,
                builder: &mut builder,
                value_lookup: &mut mir_to_ir_lookup,
                source: &function,
                sources: &sources,
                block_lookup: &mut ir_block_lookup,
                stack_slot_lookup: &mut stack_slot_lookup,
                t_functions: &t_funcs,
                types: &types,
                variable_set: &mut variable_set,
            }
            .generate();
    
            println!("{}", ctx.func.display());
    
            let id = func_lookup[func].unwrap();
            module.define_function(id, &mut ctx).unwrap();
    
            stack_slot_lookup.clear();
            mir_to_ir_lookup.clear();
            ir_block_lookup.clear();
            variable_set.clear();
        }
    }

    // linking
    let binary = module.finish().emit().unwrap();
    std::fs::write("catalyst.o", &binary).unwrap();

    if input.enabled("no-link") {
        return Ok(());
    }

    let linker = input.field("linker").unwrap_or("link");
    let status = Command::new(linker)
        .arg("catalyst.o")
        .arg("libvcruntime.lib")
        .arg("libcmt.lib")
        .arg("libucrt.lib")
        .arg("/entry:main")
        .status()
        .unwrap();
    
    std::fs::remove_file("catalyst.o").unwrap();

    assert!(status.success(), "{status:?}");

    Ok(())
}