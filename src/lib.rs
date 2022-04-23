#![feature(result_option_inspect)]
#![feature(let_else)]

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

/// compiles the catalyst source code, taking command like args as input,
/// complete compilation only works on windows with msvc and you have to invoke
/// compiler inside Native Tools Command Prompt
pub fn compile() {
    let total_now = std::time::Instant::now();

    // cli
    let input = CmdInput::new();

    assert!(&input.args()[0] == "c");

    let path = Path::new(&input.args()[1]);

    // lexer
    let mut sources = Sources::new();
    let mut builtin_source = BuiltinSource::new(&mut sources);

    // parser
    let mut ast = ast::Data::new();
    let mut ast_temp = Stack::new();

    // modules
    let mut scope = Scope::new();
    let mut module_map = Map::new();
    let mut unit_load_ctx = LoaderContext::new();
    let mut modules = Modules::new();
    let mut units = Units::new();

    // errors
    let mut diagnostics = errors::Diagnostics::new();
    
    let _scope_item_lexicon = {
        let mut map = ItemLexicon::new();

        map.register::<module::Module>("module");
        map.register::<typec::Ty>("type");
        map.register::<Func>("function");
        map.register::<Tir>("tir");

        map
    };
    
    let module_order = {
        let mut module_frontier = VecDeque::new();
    
        let unit_order = unit::Loader {
            sources: &mut sources,
            units: &mut units,
            ctx: &mut unit_load_ctx,
            diagnostics: &mut diagnostics,
        }
        .load_units(path)
        .unwrap_or_default();

        let mut module_order = vec![];

        for unit in unit_order {
            let Ok(local_module_order) =
                module::Loader {
                    sources: &mut sources,
                    modules: &mut modules,
                    units: &mut units,
                    frontier: &mut module_frontier,
                    ctx: &mut unit_load_ctx,
                    map: &mut module_map,
                    diagnostics: &mut diagnostics,
                }
                .load_unit_modules(unit) else {
                    continue;
                };

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
    
    const NOTHING: Ty = Ty(0);
    const BOOL: Ty = Ty(1);

    let total_type_check;

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

                for name in "-".split(' ') {
                    match kind {
                        ty::Kind::Bool if !"".contains(name) => {
                            continue;
                        }
                        ty::Kind::Int(_) if !"-".contains(name) => {
                            continue;
                        }
                        _ => {}
                    }

                    let span = builtin_source.make_span(&mut sources, name);
                    let sig = {
                        let args = t_types.cons.push(&[ty]);
                        let ret = ty;
                        typec::Signature {
                            args,
                            ret,
                            call_conv: Span::default(),
                        }
                    };

                    let id = {
                        let name = sources.display(span);
                        typec::func::Builder::unary_id(id, ID::new(name))
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
                        let args = t_types.cons.push(&[ty, ty]);
                        let ret = if "< > <= >= == !=".contains(name) {
                            BOOL
                        } else {
                            ty
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
        let mut t_temp = Stack::new();

        let total_type_check_now = std::time::Instant::now();

        for module in module_order {
            let source = modules[module].source;
    
            for item in builtin_items.iter() {
                scope.insert(&mut diagnostics, source, item.id, item.to_scope_item()).unwrap();
            }
    
            ast.clear();
            let inter_state = Parser::parse_imports(&sources, &mut diagnostics, &mut ast, &mut ast_temp, source);
    
            scope.dependencies.clear();
            if let Some(imports) = ModuleImports::new(&ast, &sources).imports() {
                for import in imports {
                    let name = sources.display(import.name);
                    let &dep = module_map.get((name, module)).unwrap();
                    for item in modules[dep].items.iter() {
                        drop(scope.insert(&mut diagnostics, source, item.id, item.to_scope_item()));
                    }
                    scope.dependencies.push(modules[dep].source);
                }
            }
    
            ast.clear();
            Parser::parse_code_chunk(&sources, &mut diagnostics, &mut ast, &mut ast_temp, inter_state);
    
            //println!("{}", ast::FileDisplay::new(&ast, &sources[source].content));
    
            drop(Collector {
                nothing: NOTHING,
                scope: &mut scope,
                funcs: &mut t_funcs,
                types: &mut t_types,
                modules: &mut modules,
                func_ast: &mut func_ast,
                sources: &sources,
                ast: &ast,
                type_ast: &mut type_ast,
                module,
                diagnostics: &mut diagnostics,
            }
            .collect_items(ast.elements()));
    
            for ty in modules[module]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Ty>())
            {
                drop(typec::ty::Builder {
                    scope: &mut scope,
                    types: &mut t_types,
                    sources: &sources,
                    ast: &ast,
                    type_ast: &type_ast,
                    graph: &mut t_graph,
                    diagnostics: &mut diagnostics,
                    ty
                }
                .build());
            }
    
            for func in modules[module]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Func>())
            {

                if (typec::func::Builder {
                    nothing: NOTHING,
                    bool: BOOL,
                    scope: &mut scope,
                    types: &mut t_types,
                    sources: &sources,
                    ast: &ast,
                    func,
                    funcs: &mut t_funcs,
                    func_ast: &func_ast,
                    body: &mut bodies[func],
                    temp: &mut t_temp,
                    diagnostics: &mut diagnostics,
                }
                .build().is_err()) {
                    continue;
                };    
                
                //println!("{}", typec::tir::Display::new(&t_types, &sources, &bodies[func], t_funcs[func].body));
            }
    
            scope.clear();
        }

        total_type_check = total_type_check_now.elapsed();
    }


    let errors = {
        let mut errors = String::new();
        
        diagnostics
            .iter::<parser::Error>()
            .map(|errs| errs
                .for_each(|err| drop(err.display(&sources, &mut errors)))
            );
        
        diagnostics
            .iter::<modules::Error>()
            .map(|errs| errs
                .for_each(|err| panic!("{:?}", err))
            );

        diagnostics
            .iter::<typec::Error>()
            .map(|errs| errs
                .for_each(|err| drop(err.display(&sources, &t_types, &mut errors)))
            );

        errors
    };

    if !errors.is_empty() {
        println!("{errors}");
        return;
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
    let mut has_sret = EntitySet::new();
    
    /* declare function headers */ {
        for (id, ent) in t_funcs.iter() {
            let name = sources.display(ent.name);
            let linkage = match ent.kind {
                typec::func::Kind::Local | typec::func::Kind::Owned(_) => Linkage::Export,
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
                has_sret.insert(id);
            }
    
            let func = module
                .declare_function(name, linkage, &ctx.func.signature)
                .unwrap();
    
            ctx.func.signature.clear(CallConv::Fast);
    
            func_lookup[id] = func.into();
        }
    }

    let mut total_definition = std::time::Duration::new(0, 0); 
    let mut total_translation = std::time::Duration::new(0, 0); 
    let mut total_generation = std::time::Duration::new(0, 0); 

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
    
            let now = std::time::Instant::now();
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
                nothing: NOTHING,
                ptr_ty,
                return_dest: None,
                has_sret: &has_sret,
                sources: &sources,
            }
            .translate_func()
            .unwrap();
            total_translation += now.elapsed();

    
            //println!("{}", mir::Display::new(&sources, &function, &t_types));
            let now = std::time::Instant::now();
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
            total_generation += now.elapsed();

            //println!("{}", ctx.func.display());
            
            let id = func_lookup[func].unwrap();

            let now = std::time::Instant::now();
            module.define_function(id, &mut ctx).unwrap();
            total_definition += now.elapsed();
    
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
        return;
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

    let total = total_now.elapsed();

    println!("parsing and generating tir: {:?}", total_type_check);
    println!("translating tir to mir: {:?}", total_translation);
    println!("translating mir to cir: {:?}", total_generation);
    println!("translating cir to bite code: {:?}", total_definition);
    println!("compilation time: {:?}", total);
}