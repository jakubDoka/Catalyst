#![feature(result_option_inspect)]
#![feature(let_else)]

use cli::CmdInput;
use cranelift_codegen::packed_option::ReservedValue;
use std::str::FromStr;
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::Flags;
use cranelift_codegen::Context;
use cranelift_entity::{SecondaryMap, EntitySet};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use gen::*;
use instance::*;
use lexer::*;
use modules::*;
use parser::*;
use typec::*;
use std::{collections::VecDeque, path::Path, fmt::Write};

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
    let mut ast_temp = FramedStack::new();

    // modules
    let mut scope = Scope::new();
    let mut module_map = Map::new();
    let mut unit_load_ctx = LoaderContext::new();
    let mut modules = Modules::new();
    let mut units = Units::new();

    // errors
    let mut diagnostics = errors::Diagnostics::new();
    
    let scope_item_lexicon = {
        let mut map = ItemLexicon::new();

        map.register::<Source>("module");
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

    let (isa, triple) = {
        let setting_builder = cranelift_codegen::settings::builder();
        let flags = Flags::new(setting_builder);
    
        let target_triple = input
            .field("target")
            .map_or_else(
                || target_lexicon::Triple::host(), 
                |target| target_lexicon::triple!(target),     
            );
        
        (
            cranelift_codegen::isa::lookup(target_triple.clone())
                .unwrap()
                .finish(flags)
                .unwrap(),
            target_triple,
        )
    };

    // typec
    let mut t_graph = GenericGraph::new();
    let mut t_types = typec::Types::new(&mut t_graph, &mut sources, &mut builtin_source);
    let mut t_funcs = typec::Funcs::new();
    
    const NOTHING: Ty = Ty(0);
    const BOOL: Ty = Ty(1);
    const ANY: Ty = Ty(2);

    let total_type_check;

    let mut bodies = SecondaryMap::new();
    /* perform type checking and build tir */ {
        let builtin_items = typec::create_builtin_items(&mut t_types, &mut t_funcs, &mut sources, &mut builtin_source);

        let mut t_temp = FramedStack::new();
        let mut c_ctx = collector::Context::new();

        let total_type_check_now = std::time::Instant::now();

        let mut func_buffer = vec![];
        let mut ty_buffer = vec![];

        for source in module_order {    
            for item in builtin_items.iter() {
                scope.insert(&mut diagnostics, source, item.id, item.to_scope_item()).unwrap();
            }
    
            ast.clear();
            let inter_state = Parser::parse_imports(&sources, &mut diagnostics, &mut ast, &mut ast_temp, source);
    
            scope.dependencies.clear();
            if let Some(imports) = ModuleImports::new(&ast, &sources).imports() {
                for import in imports {
                    let nick = sources.display(import.nick);
                    let Some(&dep) = module_map.get((nick, source)) else {
                        continue; // recovery, module might not exist due to previous recovery
                    };
                    scope.insert(&mut diagnostics, source, nick, scope::Item::new(dep, import.nick)).unwrap();
                    for item in modules[dep].items.iter() {
                        drop(scope.insert(&mut diagnostics, source, item.id, item.to_scope_item()));
                    }
                    scope.dependencies.push((dep, import.nick));
                }
            }
    
            ast.clear();
            Parser::parse_code_chunk(&sources, &mut diagnostics, &mut ast, &mut ast_temp, inter_state);
    
            //println!("{}", ast::FileDisplay::new(&ast, &sources[source].content));
    
            drop(Collector {
                nothing: NOTHING,
                any: ANY,
                scope: &mut scope,
                funcs: &mut t_funcs,
                types: &mut t_types,
                modules: &mut modules,
                sources: &sources,
                ast: &ast,
                module: source,
                ctx: &mut c_ctx,
                diagnostics: &mut diagnostics,
            }
            .collect_items(ast.elements()));
    
            drop(typec::func::Builder {
                nothing: NOTHING,
                bool: BOOL,
                scope: &mut scope,
                types: &mut t_types,
                sources: &sources,
                ast: &ast,
                funcs: &mut t_funcs,
                ctx: &mut c_ctx,
                temp: &mut t_temp,
                modules: &mut modules,
                diagnostics: &mut diagnostics,
                
                // does not matter
                body: &mut typec::tir::Data::default(), 
                func: Default::default(),
            }
            .verify_bound_impls());

            ty_buffer.extend(modules[source]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Ty>()));

            for ty in ty_buffer.drain(..) {
                drop(typec::ty::Builder {
                    scope: &mut scope,
                    types: &mut t_types,
                    sources: &sources,
                    ast: &ast,
                    ctx: &mut c_ctx,
                    graph: &mut t_graph,
                    modules: &mut modules,
                    diagnostics: &mut diagnostics,
                    ty
                }
                .build());
            }

            func_buffer.extend(modules[source]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Func>()));
    
            for func in func_buffer.drain(..) {
                if (typec::func::Builder {
                    nothing: NOTHING,
                    bool: BOOL,
                    scope: &mut scope,
                    types: &mut t_types,
                    sources: &sources,
                    ast: &ast,
                    func,
                    funcs: &mut t_funcs,
                    ctx: &mut c_ctx,
                    body: &mut bodies[func],
                    temp: &mut t_temp,
                    modules: &mut modules,
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

    for _ in t_graph.len()..t_types.ents.len() {
        t_graph.close_node();
    }


    let errors = {
        let mut errors = String::new();
        
        diagnostics
            .iter::<parser::Error>()
            .map(|errs| errs
                .for_each(|err| err.display(&sources, &mut errors).unwrap())
            );
        
        diagnostics
            .iter::<modules::Error>()
            .map(|errs| errs
                .for_each(|err| err.display(&sources, &scope_item_lexicon, &units, &mut errors).unwrap())
            );

        diagnostics
            .iter::<typec::Error>()
            .map(|errs| errs
                .for_each(|err| err.display(&sources, &t_types, &mut errors).unwrap())
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
    let mut seen_entry = false;
    
    /* declare function headers */ {
        let mut name_buffer = String::with_capacity(1024);
        for (id, ent) in t_funcs.iter() {
            let Some(linkage) = gen::func_linkage(ent.kind) else {
                continue;
            };
            
            let popper = if let typec::func::Kind::Instance(_) = ent.kind {
                let popper = t_types.push_params(ent.sig.params);
                for (&param, &real_param) in t_types.active_params(&popper).iter().zip(t_types.args.get(ent.sig.params)) {
                    types.ents[param] = types.ents[real_param];
                }
                Some(popper)
            } else {
                if !ent.sig.params.is_reserved_value() {
                    continue;
                }
                None
            };

            name_buffer.clear();
            if ent.flags.contains(typec::func::Flags::ENTRY) {
                name_buffer.push_str("main");
                if seen_entry {
                    todo!("emit error since multiple entries in executable cannot exist")
                }
                seen_entry = true;
            } else if linkage == Linkage::Import {
                name_buffer.push_str(sources.display(ent.name));
            } else {
                write!(name_buffer, "{}", id.0).unwrap();
            }

            let returns_struct = instance::func::Translator::translate_signature(
                &ent.sig,
                &mut ctx.func.signature,
                &types,
                &t_types,
                &sources,
                system_call_convention,
                ent.flags.contains(typec::func::Flags::ENTRY),
            )
            .unwrap();

            if let Some(popper) = popper {
                t_types.pop_params(popper);
            }
    
            if returns_struct {
                has_sret.insert(id);
            }
    
            let func = module
                .declare_function(&name_buffer, linkage, &ctx.func.signature)
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
        
        for (id, ent) in t_funcs.iter() {
            if gen::func_linkage(ent.kind).map(|l| l == Linkage::Import).unwrap_or(true) {
                continue;
            }

            let (popper, func) = if let typec::func::Kind::Instance(func) = ent.kind {
                let popper = t_types.push_params(ent.sig.params);
                for (&param, &real_param) in t_types.active_params(&popper).iter().zip(t_types.args.get(ent.sig.params)) {
                    types.ents[param] = types.ents[real_param];
                }
                (Some(popper), func)
            } else {
                if !ent.sig.params.is_reserved_value() {
                    continue;
                }
                (None, id)
            };
            
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

            if let Some(popper) = popper {
                t_types.pop_params(popper);
            }
    
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
                t_types: &t_types,
            }
            .generate();
            total_generation += now.elapsed();

            // println!("{}", sources.display(ent.name));
            // println!("{}", ctx.func.display());
            
            let id = func_lookup[id].unwrap();

            let now = std::time::Instant::now();
            module.define_function(id, &mut ctx).unwrap();
            total_definition += now.elapsed();
    
            stack_slot_lookup.clear();
            mir_to_ir_lookup.clear();
            ir_block_lookup.clear();
            variable_set.clear();
        }
    }

    let binary = module.finish().emit().unwrap();
    std::fs::write("catalyst.o", &binary).unwrap();

    if input.enabled("no-link") {
        return;
    }

    // let status = cc::windows_registry::find(&triple.to_string(), "link.exe")
    //     .unwrap()
    //     .arg("catalyst.o")
    //     .arg("libcmt.lib")
    //     .arg("libucrt.lib")
    //     .arg("/entry:main")
    //     .status()
    //     .unwrap();
    
    let status = cc::windows_registry::find(&triple.to_string(), "link.exe")
        .unwrap()
        .arg("catalyst.o")
        .arg("ucrt.lib")
        .arg("/entry:main")
        .status()
        .unwrap();
    
    // linking
    std::fs::remove_file("catalyst.o").unwrap();

    assert!(status.success(), "{status:?}");

    let total = total_now.elapsed();

    println!("parsing and generating tir: {:?}", total_type_check);
    println!("translating tir to mir: {:?}", total_translation);
    println!("translating mir to cir: {:?}", total_generation);
    println!("translating cir to bite code: {:?}", total_definition);
    println!("compilation time: {:?}", total);
}