#![feature(result_option_inspect)]

use std::process::Command;
use std::{collections::VecDeque, path::Path};
use cranelift_codegen::isa::CallConv;
use modules::module::{ModuleImports, self};
use typec::ty;
use cli::CmdInput;
use cranelift_codegen::Context;
use cranelift_codegen::settings::Flags;
use cranelift_entity::SecondaryMap;
use cranelift_frontend::{FunctionBuilderContext, FunctionBuilder};
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectModule, ObjectBuilder};
use gen::logic::Generator;
use instance::logic::{FunctionTranslator, TypeTranslator};
use lexer::{Sources, Map, Span, SourcesExt, BuiltinSource, ID};
use modules::{logic::{Modules, UnitLoaderContext, Units, UnitLoader, ModuleLoader}, scope::Scope};
use parser::{ast, Parser};
use typec::{Collector, Func, FuncBuilder};

pub fn compile() {
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
    
    let module_order = {
        let unit_order = UnitLoader {
            sources: &mut sources,
            units: &mut units,
            ctx: &mut unit_load_ctx,
        }
        .load_units(path)
        .unwrap();
      
        let mut module_order = vec![];
        
        for unit in unit_order {
            let local_module_order = ModuleLoader {
                sources: &mut sources,
                modules: &mut modules,
                units: &mut units,
                frontier: &mut module_frontier,
                ctx: &mut unit_load_ctx,
                map: &mut module_map,
            }
            .load_unit_modules(unit)
            .unwrap();
            
            module_order.extend(local_module_order.into_iter().rev());
        }

        module_order.reverse();

        module_order
    };

    for &module in &module_order {
        let source = modules[module].source;
        let path = sources[source].path.clone();
        println!("{}", path.to_str().unwrap());
    }

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
    let mut t_functions = typec::Functions::new();

    let builtin_items: Vec<module::Item> = {
        let builtin_types = [
            ("int", ty::Kind::Int(-1))
        ];

        let builtin_binary_operators = "+ - * /";

        let mut vec = Vec::with_capacity(builtin_types.len() + builtin_binary_operators.split(' ').count());

        for (name, kind) in builtin_types {
            let span = builtin_source.make_span(&mut sources, name);
            let id = name.into();
            
            let ty = {
                let ent = ty::Ent::new(kind, id);
                t_types.add(ent)
            };
            
            let item = module::Item::new(id, ty, span);
            vec.push(item);

            for name in builtin_binary_operators.split(' ') {
                let name = builtin_source.make_span(&mut sources, name);
                let sig = {
                    let args = t_types.add_slice(&[ty, ty]);
                    let ret = ty.into();
                    typec::Signature { args, ret, call_conv: Span::default() }
                };
                
                let id = {
                    let name = sources.display(name);
                    ID::new("<binary>") + ID::new(name) + id
                };

                let func = {
                    let ent = typec::func::Ent {
                        sig,
                        name,
                        id,
                        kind: typec::func::Kind::Builtin,
                        ..Default::default()                    
                    };
                    t_functions.add(ent)
                };

                let item = module::Item::new(id, func, name);
                vec.push(item);
            }
        }

        vec
    };
    
    for module in module_order {        
        let source = modules[module].source;
        let content = &sources[source].content;

        for item in builtin_items.iter() {
            scope.insert(source, item.id, item.to_scope_item()).unwrap();
        }

        ast.clear();
        let inter_state = Parser::parse_imports(content, &mut ast, &mut ast_temp, source)
            .unwrap();

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
        Parser::parse_code_chunk(content, &mut ast, &mut ast_temp, inter_state)
            .unwrap();
        
        Collector {
            scope: &mut scope,
            functions: &mut t_functions,
            types: &mut t_types,
            modules: &mut modules,
            sources: &sources,
            ast: &ast,
        }
        .collect_items(module)
        .unwrap();

        for func in modules[module].items.iter().filter_map(|i| i.kind.may_read::<Func>()) {
            FuncBuilder {
                scope: &mut scope,
                functions: &mut t_functions,
                types: &mut t_types,
                sources: &sources,
                ast: &ast,
                func,
            }
            .build()
            .inspect_err(|e| eprintln!("{}", sources.display(e.span)))
            .unwrap();

            println!("{}", t_functions.display(func, &sources, &ast));
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
}