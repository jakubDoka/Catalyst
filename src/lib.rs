use std::process::Command;
use std::{collections::VecDeque, path::Path};
use modules::scope;
use typec::ty;
use cli::CmdInput;
use cranelift_codegen::Context;
use cranelift_codegen::settings::Flags;
use cranelift_entity::SecondaryMap;
use cranelift_frontend::{FunctionBuilderContext, FunctionBuilder};
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectModule, ObjectBuilder};
use gen::logic::Generator;
use instance::logic::Translator;
use lexer::{Sources, Map, Source, Span};
use modules::{logic::{Modules, UnitLoaderContext, Units, UnitLoader, ModuleLoader}, scope::Scope};
use parser::{ast, Parser};
use typec::{Collector, Func, Builder};

pub fn compile() {
    // cli
    let input = CmdInput::new();
    
    assert!(&input.args()[0] == "c");

    let path = Path::new(&input.args()[1]);

    // lexer
    let mut sources = Sources::new();
    
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
    let mut t_functions = typec::Functions::new();

    let int = t_types.add(ty::Ent::new(ty::Kind::Int(-1), "int".into()));
    scope.insert(Source(0), "int", scope::Item::new(int, Span::new(Source(0), 0, 0))).unwrap();

    for module in module_order {
        let source = modules[module].source;
        let content = &sources[source].content;

        ast.clear();
        let inter_state = Parser::parse_imports(content, &mut ast, &mut ast_temp, source)
            .unwrap();
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
            Builder {
                scope: &mut scope,
                functions: &mut t_functions,
                types: &mut t_types,
                sources: &sources,
                ast: &ast,
            }
            .build_function_ir(func)
            .unwrap();
        }
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
    let ptr_ty = module.isa().pointer_type(); 
    let system_call_convention = module.isa().default_call_conv();
    for (func, _) in t_functions.ents.iter() {
        tir_to_mir_lookup.clear();
        Translator {
            ptr_ty,
            system_call_convention,
            value_lookup: &mut tir_to_mir_lookup,
            function: &mut function,
            t_functions: &t_functions,
            t_types: &t_types,
            sources: &sources,
        }
        .translate_func(func)
        .unwrap();
        
        ctx.clear();
        mir_to_ir_lookup.clear();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);
        Generator {
            builder: &mut builder,
            value_lookup: &mut mir_to_ir_lookup,
            source: &function,
        }
        .generate();

        // "main" is temporary, now I see whats missing
        let func = module.declare_function("main", Linkage::Export, &ctx.func.signature)
            .unwrap();
        module.define_function(func, &mut ctx).unwrap();
    }

    // linking
    let binary = module.finish().emit().unwrap();
    std::fs::write("catalyst.o", &binary).unwrap();
    
    let linker = input.field("linker").unwrap_or("link");
    let status = Command::new(linker)
        .arg("catalyst.o")
        .arg("/entry:main")
        .status()
        .unwrap();
    
    assert!(status.success(), "{status:?}");
}