#![feature(result_option_inspect)]

use std::process::Command;
use std::{collections::VecDeque, path::Path};
use cranelift_codegen::isa::CallConv;
use modules::module::{ModuleImports, self};
use modules::scope::ScopeItemLexicon;
use typec::builder::Builder;
use typec::{ty, Ty};
use cli::CmdInput;
use cranelift_codegen::Context;
use cranelift_codegen::settings::Flags;
use cranelift_entity::SecondaryMap;
use cranelift_frontend::{FunctionBuilderContext, FunctionBuilder};
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectModule, ObjectBuilder};
use gen::logic::Generator;
use instance::logic::{FunctionTranslator, TypeTranslator};
use lexer::{Sources, Map, Span, SourcesExt, BuiltinSource, ID, ListPoolExt};
use modules::{logic::{Modules, UnitLoaderContext, Units, UnitLoader, ModuleLoader}, scope::Scope};
use parser::{ast, Parser};
use typec::{Collector, Func, FuncBuilder};

macro_rules! unwrap {
    ($expr:expr, $err:ident, $mapping:expr) => {
        match ($expr) {
            Ok(p) => p,
            Err($err) => {
                return Err($mapping);
            },
        }
    };
}

pub fn compile() -> Result<(), Box<dyn std::fmt::Display>> {
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
    let mut module_frontier = VecDeque::new();
    let mut unit_load_ctx = UnitLoaderContext::new();
    let mut modules = Modules::new();
    let mut units = Units::new();
    let scope_item_lexicon = {
        let mut map = ScopeItemLexicon::new();

        for (id, name) in [
            (std::any::TypeId::of::<module::Module>(), "module"),
            (std::any::TypeId::of::<Ty>(), "type"),
            (std::any::TypeId::of::<Func>(), "function"),
        ] {
            map.insert(id, name);
        }

        map
    };

    let module_order = {
        let unit_order = unwrap!(UnitLoader {
                    sources: &mut sources,
                units: &mut units,
                ctx: &mut unit_load_ctx,
            }
            .load_units(path),
            err,
            Box::new(modules::error::Display::new(sources, err, modules, units, scope_item_lexicon))
        );

      
        let mut module_order = vec![];
        
        for unit in unit_order {
            let local_module_order = unwrap!(ModuleLoader {
                    sources: &mut sources,
                    modules: &mut modules,
                    units: &mut units,
                    frontier: &mut module_frontier,
                    ctx: &mut unit_load_ctx,
                    map: &mut module_map,
                }
                .load_unit_modules(unit),
                err,
                Box::new(modules::error::Display::new(sources, err, modules, units, scope_item_lexicon))
            );
            
            module_order.extend(local_module_order.into_iter().rev());
        }

        module_order.reverse();

        module_order
    };

    // settings
    let setting_builder = cranelift_codegen::settings::builder();
    let flags = Flags::new(setting_builder);

    // isa
    // let target_triple_str = input.field("target").unwrap_or("unknown-unknown-unknown");
    let target_triple = target_lexicon::Triple::host();
    let target_isa = cranelift_codegen::isa::lookup(target_triple)
        .unwrap()
        .finish(flags)
        .unwrap();

    // typec
    let mut t_types = typec::Types::new();
    let mut t_functions = typec::Funcs::new();

    let builtin_items: Vec<module::Item> = {
        let builtin_types = [
            ("int", ty::Kind::Int(-1)),
            ("bool", ty::Kind::Bool),
        ];

        let mut vec = vec![];

        for (name, kind) in builtin_types {
            let span = builtin_source.make_span(&mut sources, name);
            let id = name.into();
            
            let ty = {
                let ent = ty::Ent::new(kind, id);
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
                    _ => {}, 
                }

                let span = builtin_source.make_span(&mut sources, name);
                let sig = {
                    let args = t_types.cons.list(&[ty, ty]);
                    let ret = if "< > <= >= == !=".contains(name) {
                        Ty(1).into() // bool
                    } else {
                        ty.into()
                    };
                    typec::Signature { args, ret, call_conv: Span::default() }
                };
                
                let id = {
                    let name = sources.display(span);
                    ID::new("<binary>") + ID::new(name) + id
                };

                let func = {
                    let ent = typec::func::Ent {
                        sig,
                        name: span,
                        id,
                        kind: typec::func::Kind::Builtin,
                        ..Default::default()                    
                    };
                    t_functions.ents.push(ent)
                };

                let item = module::Item::new(id, func, span);
                vec.push(item);
            }
        }

        vec
    };

    let mut loops = vec![];
    
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
        
        unwrap!(Collector {
                scope: &mut scope,
                functions: &mut t_functions,
                types: &mut t_types,
                modules: &mut modules,
                sources: &sources,
                ast: &ast,
            }
            .collect_items(module),
            err,
            Box::new(typec::error::Display::new(sources, err, modules, units, scope_item_lexicon))
        );

        for func in modules[module].items.iter().filter_map(|i| i.kind.may_read::<Func>()) {            
            unwrap!(FuncBuilder {
                    scope: &mut scope,
                    builder: &mut Builder { 
                        funcs: & mut t_functions,
                        func: func, 
                        block: None, 
                    },
                    loops: &mut loops,
                    types: &mut t_types,
                    sources: &sources,
                    ast: &ast,
                }
                .build()
                .inspect_err(|e| eprintln!("{}", sources.display(e.span))),
                err,
                Box::new(typec::error::Display::new(sources, err, modules, units, scope_item_lexicon))
            );

            println!("{}", t_functions.display(func, &t_types, &sources, &ast));
        }

        scope.clear();
    }

    // cranelift
    let mut func_builder_ctx = FunctionBuilderContext::new();
    let mut ctx = Context::new();

    // module
    let builder = ObjectBuilder::new(target_isa, "catalyst", cranelift_module::default_libcall_names())
        .unwrap();
    let mut module = ObjectModule::new(builder);

    // instance
    let mut function = instance::Function::new();

    let mut tir_to_mir_lookup = SecondaryMap::new();
    let mut mir_to_ir_lookup = SecondaryMap::new();
    let mut mir_block_lookup = SecondaryMap::new();
    let mut ir_block_lookup = SecondaryMap::new();
    let mut func_lookup = SecondaryMap::new();
    let mut repr_lookup = SecondaryMap::new();
    let ptr_ty = module.isa().pointer_type(); 
    let system_call_convention = module.isa().default_call_conv();
    
    TypeTranslator {
        repr_lookup: &mut repr_lookup,
        t_types: &t_types,
        ptr_ty,
    }
    .translate()
    .unwrap();

    // declare everything
    for (id, ent) in t_functions.ents.iter() {
        let name = sources.display(ent.name);
        let linkage = match ent.kind {
            typec::func::Kind::Local => Linkage::Export,
            typec::func::Kind::Builtin => continue,
            typec::func::Kind::External => Linkage::Import,
        };
        
        FunctionTranslator::translate_signature(
            &ent.sig,
            &mut ctx.func.signature, 
            &sources,
            &repr_lookup,
            &t_types, 
            system_call_convention
        ).unwrap();

        let func = module.declare_function(name, linkage, &ctx.func.signature)
            .unwrap();
            
        ctx.func.signature.clear(CallConv::Fast);

        func_lookup[id] = func.into();
    }

    for (func, ent) in t_functions.ents.iter() {
        if ent.kind != typec::func::Kind::Local {
            continue;
        }

        tir_to_mir_lookup.clear();
        FunctionTranslator {
            system_call_convention,
            value_lookup: &mut tir_to_mir_lookup,
            function: &mut function,
            block_lookup: &mut mir_block_lookup,
            t_functions: &t_functions,
            repr_lookup: &repr_lookup,
            t_types: &t_types,
            sources: &sources,
        }
        .translate_func(func)
        .unwrap();
                
        ctx.clear();
        mir_to_ir_lookup.clear();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);
        Generator {
            module: &mut module,
            block_lookup: &mut ir_block_lookup,
            function_lookup: &mut func_lookup,
            builder: &mut builder,
            value_lookup: &mut mir_to_ir_lookup,
            source: &function,
            t_functions: &t_functions,
            sources: &sources,
        }
        .generate();

        
        println!("{}", ctx.func.display());

        let id = func_lookup[func].unwrap();
        module.define_function(id, &mut ctx).unwrap();

        tir_to_mir_lookup.clear();
        mir_to_ir_lookup.clear();
        mir_block_lookup.clear();
        ir_block_lookup.clear();
    }

    // linking
    let binary = module.finish().emit().unwrap();
    std::fs::write("catalyst.o", &binary).unwrap();
    
    let linker = input.field("linker").unwrap_or("link");
    let status = Command::new(linker)
        .arg("catalyst.o")
        .arg("libvcruntime.lib")
        .arg("libcmt.lib")
        .arg("libucrt.lib")
        .arg("/entry:main")
        .status()
        .unwrap();
    
    assert!(status.success(), "{status:?}");

    Ok(())
}