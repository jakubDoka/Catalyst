#![feature(result_option_inspect)]
#![feature(let_else)]
#![feature(bool_to_option)]
#![feature(let_chains)]

use cli::CmdInput;
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Flags, Configurable};
use cranelift_codegen::Context;
use cranelift_entity::EntitySet;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module, FuncOrDataId};
use cranelift_object::{ObjectBuilder, ObjectModule};

use gen::*;
use incr::{Incr, IncrFuncData, IncrRelocRecord};
use instance::*;
use instance::repr::ReprInstancing;
use modules::*;
use modules::module::ModuleImports;
use parser::*;
use lexer_types::*;
use module_types::*;
use instance_types::*;
use storage::*;
use typec::tir::BoundVerifier;
use typec_types::*;
use typec::*;
use ast::*;

use std::str::FromStr;
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

    let incr_data_path = path.join("incr.bin");
    let mut incr = Incr::load("1.0", &incr_data_path).unwrap_or_default();
    incr.version = "1.0".to_string();

    // lexer
    let mut sources = Sources::new();
    let mut builtin_source = BuiltinSource::new(&mut sources);

    // parser
    let mut ast = AstData::new();
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
        map.register::<Ty>("type");
        map.register::<Func>("function");
        map.register::<Tir>("tir");

        map
    };

    let module_order = {
        let mut module_frontier = VecDeque::new();

        let unit_order = unit::UnitBuilder {
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
                ModuleBuilder {
                    sources: &mut sources,
                    modules: &mut modules,
                    units: &mut units,
                    frontier: &mut module_frontier,
                    ctx: &mut unit_load_ctx,
                    map: &mut module_map,
                    diagnostics: &mut diagnostics,
                    incr: &mut incr,
                }
                .load_unit_modules(unit) else {
                    continue;
                };

            module_order.extend(local_module_order.into_iter().rev());
        }

        module_order.reverse();

        for (i, &id) in module_order.iter().enumerate() {
            modules[id].ordering = i;
        }            

        module_order
    };

    incr.reduce(&module_map, &modules, &module_order);

    let (isa, triple) = {
        let mut setting_builder = cranelift_codegen::settings::builder();
        // setting_builder.set("enable_verifier", "false").unwrap();
        setting_builder.set("enable_verifier", "true").unwrap();
        let flags = Flags::new(setting_builder);

        let target_triple = input.field("target").map_or_else(
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
    let mut graph = GenericGraph::new();
    let mut types = Types::new();
    let builtin_types = BuiltinTypes::new(&mut graph, &mut sources, &mut builtin_source, &mut types);
    let mut t_funcs = Funcs::new();
    let mut ty_lists = TyLists::new();
    let mut instances = Instances::new();
    let mut func_instances = FuncInstances::new();
    let mut sfields = SFields::new();
    let mut sfield_lookup = SFieldLookup::new();
    let mut tfunc_lists = TFuncLists::new();
    let mut bound_impls = BoundImpls::new();
    let mut to_compile = ToCompile::new();

    let total_type_check;

    let mut bodies = SecondaryMap::new();
    /* perform type checking and build tir */
    {
        let b_source = builtin_source.source;
        typec::create_builtin_items(
            &mut types, 
            &mut ty_lists, 
            &builtin_types, 
            &mut t_funcs, 
            &mut sources, 
            &mut builtin_source, 
            &mut modules[b_source].items,
        );

        let mut t_temp = FramedStack::new();
        let mut c_ctx = ScopeContext::new();
        let mut body = TirData::new();

        let total_type_check_now = std::time::Instant::now();

        let mut func_buffer = vec![];
        let mut ty_buffer = vec![];

        for source in module_order {
            for item in modules[builtin_source.source].items.iter() {
                scope
                    .insert(&mut diagnostics, source, item.id, item.to_scope_item())
                    .unwrap();
            }
            ast.clear();
            let inter_state =
                Parser::parse_imports(&sources, &mut diagnostics, &mut ast, &mut ast_temp, source);

            scope.dependencies.clear();
            if let Some(imports) = ModuleImports::new(&ast, &sources).imports() {
                for import in imports {
                    let nick = sources.display(import.nick);
                    let Some(&dep) = module_map.get((nick, source)) else {
                    continue; // recovery, module might not exist due to previous recovery
                };
                    scope
                        .insert(
                            &mut diagnostics,
                            source,
                            nick,
                            ScopeItem::new(dep, import.nick),
                        )
                        .unwrap();
                    for item in modules[dep].items.iter() {
                        drop(scope.insert(&mut diagnostics, source, item.id, item.to_scope_item()));
                    }
                    scope.dependencies.push((dep, import.nick));
                }
            }

            ast.clear();
            Parser::parse_code_chunk(
                &sources,
                &mut diagnostics,
                &mut ast,
                &mut ast_temp,
                inter_state,
            );

            //println!("{}", ast::FileDisplay::new(&ast, &sources[source].content));

            drop(
                ScopeBuilder {
                    scope: &mut scope,
                    funcs: &mut t_funcs,
                    types: &mut types,
                    modules: &mut modules,
                    sources: &sources,
                    ast: &ast,
                    module: source,
                    ctx: &mut c_ctx,
                    diagnostics: &mut diagnostics,
                    ty_lists: &mut ty_lists,
                    sfields: &mut sfields,
                    sfield_lookup: &mut sfield_lookup,
                    builtin_types: &builtin_types,
                    tfunc_lists: &mut tfunc_lists,
                    instances: &mut instances,
                    bound_impls: &mut bound_impls,
                    to_compile: &mut to_compile,
                    incr: &mut incr,
                }
                .collect_items(ast.elements()),
            );

            ty_buffer.extend(
                modules[source]
                    .items
                    .iter()
                    .filter_map(|item| item.kind.may_read::<Ty>()),
            );

            for ty in ty_buffer.drain(..) {
                drop(
                    TyBuilder {
                        scope: &mut scope,
                        types: &mut types,
                        sources: &sources,
                        ast: &ast,
                        ctx: &mut c_ctx,
                        graph: &mut graph,
                        modules: &mut modules,
                        diagnostics: &mut diagnostics,
                        ty_lists: &mut ty_lists,
                        sfields: &mut sfields,
                        sfield_lookup: &mut sfield_lookup,
                        builtin_types: &builtin_types,
                        instances: &mut instances,
                        bound_impls: &mut bound_impls,
                        ty,
                    }
                    .build(),
                );
            }

            drop(
                BoundVerifier {
                    scope: &mut scope,
                    types: &mut types,
                    sources: &sources,
                    ast: &ast,
                    funcs: &mut t_funcs,
                    ctx: &mut c_ctx,
                    modules: &mut modules,
                    diagnostics: &mut diagnostics,
                    ty_lists: &mut ty_lists,
                    builtin_types: &builtin_types,
                    func_lists: &mut tfunc_lists,
                    bound_impls: &mut bound_impls,
                }
                .verify_bound_impls(),
            );

            func_buffer.extend(
                modules[source]
                    .items
                    .iter()
                    .filter_map(|item| item.kind.may_read::<Func>()),
            );

            for func in func_buffer.drain(..) {
                body.clear();
                if (TirBuilder {
                    scope: &mut scope,
                    types: &mut types,
                    sources: &sources,
                    ast: &ast,
                    func,
                    to_compile: &mut to_compile,
                    funcs: &mut t_funcs,
                    ctx: &mut c_ctx,
                    body: &mut body,
                    temp: &mut t_temp,
                    modules: &mut modules,
                    diagnostics: &mut diagnostics,
                    func_lists: &mut tfunc_lists,
                    func_instances: &mut func_instances,
                    ty_lists: &mut ty_lists,
                    instances: &mut instances,
                    sfields: &mut sfields,
                    sfield_lookup: &mut sfield_lookup,
                    builtin_types: &builtin_types,
                    bound_impls: &mut bound_impls,
                    incr: &mut incr,
                }
                .build()
                .is_err())
                {
                    continue;
                };

                bodies[func] = body.clone();
                // println!("{}", TirDisplay::new(&types, &ty_lists, &sfields, &sources, &bodies[func], t_funcs[func].body));
            }

            // println!("typecheck {}", scope.collision_rate());

            scope.clear();
        }

        total_type_check = total_type_check_now.elapsed();
    }

    for _ in graph.len()..types.len() {
        graph.close_node();
    }

    if !diagnostics.is_empty() {
        let mut errors = String::new();

        diagnostics
            .iter::<AstError>()
            .map(|errs| errs.for_each(|err| err.display(&sources, &mut errors).unwrap()));

        diagnostics.iter::<ModuleError>().map(|errs| {
            errs.for_each(|err| {
                modules::error::display(err, &sources, &scope_item_lexicon, &units, &mut errors)
                    .unwrap()
            })
        });

        diagnostics
            .iter::<TyError>()
            .map(|errs| errs.for_each(|err| typec::error::display(err, &sources, &types, &ty_lists, &mut errors).unwrap()));

        println!("{errors}");
        return;
    };

    // cranelift
    let mut func_builder_ctx = FunctionBuilderContext::new();
    let mut ctx = Context::new();

    // module
    let builder =
        ObjectBuilder::new(isa, "catalyst", cranelift_module::default_libcall_names()).unwrap();
    let mut module = ObjectModule::new(builder);

    // instance
    let mut function = FuncCtx::new();
    let mut reprs = Reprs::new();
    let mut repr_fields = ReprFields::new();
    let ptr_ty = module.isa().pointer_type();
    let system_call_convention = module.isa().default_call_conv();

    instance::repr::ReprBuilder {
        types: &types,
        repr_fields: &mut repr_fields,
        sources: &sources,
        reprs: &mut reprs,
        sfields: &sfields,
        ptr_ty,
        instances: &instances,
        ty_lists: &ty_lists,
    }
    .translate(&graph)
    .unwrap();

    let mut func_lookup = SecondaryMap::new();
    let mut replace_cache = ReplaceCache::new();
    let mut entry_id = None;
    let mut incremental_decls = Vec::with_capacity(t_funcs.len());
    let mut name_buffer = String::new();
    
    /* declare function headers */
    {
        for &func in &to_compile {
            name_buffer.clear();
            
            let ent = &t_funcs[func];

            let Some(linkage) = gen::func_linkage(ent.kind) else {
                continue;
            };

            if ent.flags.contains(TFuncFlags::ENTRY) {
                if entry_id.is_some() {
                    todo!("multiple entry functions");
                }
                entry_id = Some(ent.id);
            }

            if incr.functions.get(ent.id).is_some() {
                incremental_decls.push((ent.id, Some(func)));
                continue;
            }

            if let TFuncKind::Instance(func) = ent.kind {
                ReprInstancing {
                    types: &mut types,
                    ty_lists: &mut ty_lists,
                    instances: &mut instances,
                    sfields: &sfields,
                    sources: &sources,
                    repr_fields: &mut repr_fields,
                    reprs: &mut reprs,
                    ptr_ty,
                }.load_generic_types(
                    ent.sig.params, 
                    bodies[func].used_types, 
                    &mut replace_cache
                );
            }

            name_buffer.clear();
            
            if linkage == Linkage::Import {
                name_buffer.push_str(sources.display(ent.name));
            } else {
                ent.id.to_ident(&mut name_buffer);
            }

            instance::MirBuilder::translate_signature(
                &ent.sig,
                &mut ctx.func.signature,
                &reprs,
                &types,
                &ty_lists,
                &sources,
                system_call_convention,
                ent.flags.contains(TFuncFlags::ENTRY),
            )
            .unwrap();

            // println!("{}", sources.display(ent.name));
            // println!("{:?}", ctx.func.signature);

            let func_id = module
                .declare_function(&name_buffer, linkage, &ctx.func.signature)
                .unwrap();

            ctx.func.signature.clear(CallConv::Fast);

            replace_cache.replace(&mut types, &mut reprs);

            func_lookup[func] = PackedOption::from(func_id);
        }

        let mut i = 0;
        while i < incremental_decls.len() {
            let (id, func) = incremental_decls[i];
            i += 1;
            
            let Some(incr_func) = incr.functions.get(id) else {
                unreachable!();
            };

            name_buffer.clear();
            id.to_ident(&mut name_buffer);

            let Ok(func_id) = module.declare_function(&name_buffer, Linkage::Export, &incr_func.signature) else {
                unreachable!();
            };

            if let Some(func) = func {
                func_lookup[func] = PackedOption::from(func_id);
            }

            incremental_decls.extend(
                incr_func
                    .dependencies()
                    .map(|dep| (dep, None))
            );
        }
    }

    /* define */
    {
        let mut variable_set = EntitySet::new();
        let mut stack_slot_lookup = SecondaryMap::new();
        let mut mir_to_ir_lookup = SecondaryMap::new();
        let mut ir_block_lookup = SecondaryMap::new();
        let mut tir_mapping = SecondaryMap::new();
        let mut reloc_temp = vec![];
        let mut name_buffer = String::new();
        let mut defined = Map::new();

        while let Some(func) = to_compile.pop() {

            let ent = &t_funcs[func];

            if gen::func_linkage(ent.kind)
                .map(|l| l == Linkage::Import)
                .unwrap_or(true)
            {
                continue;
            }

            if let Some(func_incr_data) = incr.functions.get(ent.id) {
                if defined.insert(ent.id, ()).is_some() {
                    continue;
                }

                reloc_temp.clear();
                reloc_temp.extend(func_incr_data.reloc_records
                    .iter()
                    .map(|r| r.to_mach_reloc(&mut name_buffer, &module))
                );

                module.define_function_bytes(func_lookup[func].unwrap(), &func_incr_data.bytes, &reloc_temp).unwrap();

                continue;
            }

            let func = if let TFuncKind::Instance(func) = ent.kind {
                ReprInstancing {
                    types: &mut types,
                    ty_lists: &mut ty_lists,
                    instances: &mut instances,
                    sfields: &sfields,
                    sources: &sources,
                    repr_fields: &mut repr_fields,
                    reprs: &mut reprs,
                    ptr_ty,
                }.load_generic_types(
                    ent.sig.params, 
                    bodies[func].used_types, 
                    &mut replace_cache
                );
                func
            } else {
                func
            };

            tir_mapping.clear();
            function.clear();
            MirBuilder {
                system_call_convention,
                func_id: func,
                reprs: &reprs,
                types: &types,
                t_funcs: &t_funcs,
                func: &mut function,
                tir_mapping: &mut tir_mapping,
                body: &bodies[func],
                ptr_ty,
                return_dest: None,
                sources: &sources,
                ty_lists: &ty_lists,
                func_lists: &tfunc_lists,
                sfields: &sfields,
                bound_impls: &bound_impls,
                repr_fields: &repr_fields,
                builtin_types: &builtin_types,
                diagnostics: &mut diagnostics,
            }
            .translate_func()
            .unwrap();

            if !diagnostics.is_empty() {
                continue;
            }

            // println!("{}", MirDisplay::new(&sources, &ty_lists, &function, &types));
            ctx.clear();
            mir_to_ir_lookup.clear();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);
            CirBuilder {
                module: &mut module,
                function_lookup: &mut func_lookup,
                builder: &mut builder,
                value_lookup: &mut mir_to_ir_lookup,
                source: &function,
                sources: &sources,
                block_lookup: &mut ir_block_lookup,
                stack_slot_lookup: &mut stack_slot_lookup,
                t_funcs: &t_funcs,
                reprs: &reprs,
                variable_set: &mut variable_set,
                types: &types,
                ty_lists: &ty_lists,
            }
            .generate();

            replace_cache.replace(&mut types, &mut reprs);
            // println!("{}", sources.display(ent.name));
            // println!("{}", ctx.func.display());
            
            let id = func_lookup[func].unwrap();

            let mut mem = vec![];
            ctx.compile_and_emit(module.isa(), &mut mem).unwrap();
            let reloc_records = ctx.mach_compile_result.as_ref().unwrap().buffer.relocs(); 
            module.define_function_bytes(id, &mem, reloc_records).unwrap();
            
            {
                let incr_func_data = IncrFuncData {
                    signature: ctx.func.signature.clone(),
                    bytes: mem,
                    reloc_records: reloc_records
                        .iter()
                        .map(|r| IncrRelocRecord::from_mach_reloc(r, &module))
                        .collect(),
                };
                incr.functions.insert(ent.id, incr_func_data);

                let home = ent.home_module_id(&ty_lists, &modules, &types);
                incr.modules.get_mut(home).unwrap().owned_functions.insert(ent.id, ());
            }

            stack_slot_lookup.clear();
            mir_to_ir_lookup.clear();
            ir_block_lookup.clear();
            variable_set.clear();
        }
    }

    if !diagnostics.is_empty() {
        let mut errors = String::new();

        diagnostics.iter::<InstError>()
            .unwrap()
            .for_each(|err| drop(instance::error::display(&err, &types, &ty_lists, &sources, &mut errors)));
        
        println!("{errors}");
        return;
    }

    incr.save(&incr_data_path).unwrap();

    let binary = module.finish().emit().unwrap();
    std::fs::write("catalyst.o", &binary).unwrap();

    if input.enabled("no-link") {
        return;
    }

    let mut entry = "/entry:".to_string();
    entry_id.unwrap().to_ident(&mut &mut entry);

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
        .arg("/subsystem:console")
        .arg(entry)
        .status()
        .unwrap();

    // linking
    std::fs::remove_file("catalyst.o").unwrap();

    assert!(status.success(), "{status:?}");

    let total = total_now.elapsed();

    println!("parsing and generating tir: {:?}", total_type_check);
    println!("compilation time: {:?}", total);
    println!("lines of code: {:?}", sources.values().map(|s| s.mapping.line_count()).sum::<usize>());
}

#[test]
fn test_parse() {
    let vec = Vec::<i8>::with_capacity(1024);
    let vec2 = vec.clone();

    assert_eq!(vec.capacity(), vec2.capacity());
}