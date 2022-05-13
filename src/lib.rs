#![feature(result_option_inspect)]
#![feature(let_else)]
#![feature(bool_to_option)]
#![feature(let_chains)]
#![feature(result_flattening)]

use cli::CmdInput;
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Flags, Configurable};
use cranelift_codegen::Context;
use cranelift_entity::EntitySet;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module, FuncOrDataId};
use cranelift_object::{ObjectBuilder, ObjectModule};

use errors::Diagnostics;
use incr::Incr;
use instance::*;
use instance::repr::ReprInstancing;
use modules::*;
use modules::module::ModuleImports;
use parser::*;
use lexer_types::*;
use module_types::*;
use instance_types::*;
use storage::*;
use target_lexicon::Triple;
use typec::tir::BoundVerifier;
use typec_types::*;
use typec::*;
use ast::*;

use std::ops::Sub;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::{Instant, Duration, SystemTime};
use std::{collections::VecDeque, path::Path};

macro_rules! exit {
    ($expr:expr) => {
        std::process::exit($expr);
    };

    () => {
        exit!(1);
    };
}

macro_rules! time_report {
    ($message:expr) => {
        let _report = TimeReport::new($message);
    };
}

pub struct Compiler {
    // cmd
    input: CmdInput,
    subcommand: Subcommand,

    // incremental data
    incr: Incr,
    incr_path: PathBuf,

    // source data
    sources: Sources,
    builtin_source: BuiltinSource,

    // ast
    ast: AstData,
    ast_temp: FramedStack<Ast>,

    // modules
    scope: Scope,
    module_map: Map<Source>,
    loader_context: LoaderContext,
    modules: Modules,
    units: Units,
    module_order: Vec<Source>,

    // diagnostics
    diagnostics: Diagnostics,
    item_lexicon: ItemLexicon,

    // typec
    ty_graph: GenericGraph,
    types: Types,
    builtin_types: BuiltinTypes,
    funcs: Funcs,
    ty_lists: TyLists,
    instances: Instances,
    func_instances: FuncInstances,
    sfields: SFields,
    sfield_lookup: SFieldLookup,
    tfunc_lists: TFuncLists,
    bound_impls: BoundImpls,
    func_bodies: FuncBodies,
    tir_temp: FramedStack<Tir>,
    scope_context: ScopeContext,
    tir_temp_body: TirData,
    
    // work accumulators
    to_compile: ToCompile,

    // generation
    entry_id: Option<ID>,
    object_module: ObjectModule,
    triple: Triple,
    func_builder_ctx: FunctionBuilderContext,
    ctx: Context,
    function: FuncCtx,
    reprs: Reprs,
    repr_fields: ReprFields,
}

impl Compiler {
    fn new() -> Self {
        time_report!("initialization of compiler");

        let input = CmdInput::new();

        let subcommand = Subcommand::new(&input);
        
        let modified_time = get_exe_modification_time();
        let root_path = subcommand.root_path().unwrap();
        let incr_path = root_path.join("incr.bin");
        let incr = if let Some(modified_time) = modified_time {
            Incr::load(format!("{modified_time:?}"), &incr_path)
        } else {
            Incr::default()
        };
        
        let mut sources = Sources::new();
        let mut builtin_source = BuiltinSource::new(&mut sources);
        
        let mut modules = Modules::new();
        let item_lexicon = {
            let mut map = ItemLexicon::new();
    
            map.register::<Source>("module");
            map.register::<Ty>("type");
            map.register::<Func>("function");
            map.register::<Tir>("tir");
    
            map
        };

        let mut ty_graph = GenericGraph::new();
        let mut types = Types::new();
        let builtin_types = BuiltinTypes::new(
            &mut ty_graph, 
            &mut sources, 
            &mut builtin_source, 
            &mut types
        );
        let mut ty_lists = TyLists::new();
        let mut funcs = Funcs::new();


        let b_source = builtin_source.source;
        typec::create_builtin_items(
            &mut types, 
            &mut ty_lists, 
            &builtin_types, 
            &mut funcs, 
            &mut sources, 
            &mut builtin_source, 
            &mut modules[b_source].items,
        );

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

        let object_module = {
            let builder = ObjectBuilder::new(
                isa, 
                "catalyst", 
                cranelift_module::default_libcall_names()
            ).unwrap();
            ObjectModule::new(builder)
        };

        Self {            
            subcommand: Subcommand::new(&input),
            input,

            incr,
            incr_path,

            sources,
            builtin_source,

            ast: AstData::new(),
            ast_temp: FramedStack::new(),
            
            scope: Scope::new(),
            module_map: Map::new(),
            loader_context: LoaderContext::new(),
            modules,
            units: Units::new(),
            module_order: Vec::new(),
            
            diagnostics: Diagnostics::new(),
            item_lexicon,
            
            ty_graph,
            types,
            builtin_types,
            funcs,
            ty_lists,
            instances: Instances::new(),
            func_instances: FuncInstances::new(),
            sfields: SFields::new(),
            sfield_lookup: SFieldLookup::new(),
            tfunc_lists: TFuncLists::new(),
            bound_impls: BoundImpls::new(),
            func_bodies: FuncBodies::new(),
            tir_temp: FramedStack::new(),
            tir_temp_body: TirData::new(),
            scope_context: ScopeContext::new(),
            
            to_compile: ToCompile::new(),
            
            entry_id: None,
            object_module,
            triple,
            func_builder_ctx: FunctionBuilderContext::new(),
            ctx: Context::new(),
            function: FuncCtx::new(),
            reprs: Reprs::new(),
            repr_fields: ReprFields::new(),
        }
    }

    fn load_modules(&mut self) {
        time_report!("loading of modules");

        let Subcommand::Compile(path) = &self.subcommand else {
            unreachable!();
        };

        let unit_order = unit_builder!(self)
            .load_units(&path)
            .unwrap_or_default();

        let mut module_order = vec![];

        for unit in unit_order {
            let Ok(local_module_order) = module_builder!(self).load_unit_modules(unit) else {
                continue;
            };

            module_order.extend(local_module_order.into_iter().rev());
        }

        module_order.reverse();

        for (i, &id) in module_order.iter().enumerate() {
            self.modules[id].ordering = i;
        }            

        self.module_order = module_order;
    }

    fn load_builtin_scope_items(&mut self, source: Source) {
        for item in self.modules[self.builtin_source.source].items.iter() {
            self.scope
                .insert(&mut self.diagnostics, source, item.id, item.to_scope_item())
                .unwrap();
        }
    }

    fn build_scope(&mut self, source: Source) {
        self.scope.dependencies.clear();
        if let Some(imports) = ModuleImports::new(&self.ast, &self.sources).imports() {
            for import in imports {
                let nick = self.sources.display(import.nick);
                let Some(&dep) = self.module_map.get((nick, source)) else {
                continue; // recovery, module might not exist due to previous recovery
            };
                self.scope
                    .insert(
                        &mut self.diagnostics,
                        source,
                        nick,
                        ScopeItem::new(dep, import.nick),
                    )
                    .unwrap();
                for item in self.modules[dep].items.iter() {
                    drop(self.scope.insert(&mut self.diagnostics, source, item.id, item.to_scope_item()));
                }
                self.scope.dependencies.push((dep, import.nick));
            }
        }
    }

    fn build_types(&mut self, source: Source, ty_buffer: &mut Vec<Ty>) {
        ty_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Ty>()),
        );

        for ty in ty_buffer.drain(..) {
            ty_builder!(self, ty).build();            
        }
    }

    fn build_funcs(&mut self, source: Source, func_buffer: &mut Vec<Func>) {
        func_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .filter_map(|item| item.kind.may_read::<Func>()),
        );

        for func in func_buffer.drain(..) {
            self.tir_temp_body.clear();
            if tir_builder!(self, func).build().is_err() {
                continue;
            };
            self.func_bodies[func] = self.tir_temp_body.clone();
        }
    }

    fn build_tir(&mut self) {
        time_report!("building of tir");

        let mut func_buffer = vec![];
        let mut ty_buffer = vec![];

        for source in self.module_order.clone() { // it really does not matter
            self.load_builtin_scope_items(source);

            self.ast.clear();
            
            let inter_state = Parser::parse_imports(
                &self.sources,
                &mut self.diagnostics, 
                &mut self.ast, 
                &mut self.ast_temp, 
                source
            );
            
            self.build_scope(source);

            self.ast.clear();
            Parser::parse_code_chunk(
                &self.sources,
                &mut self.diagnostics,
                &mut self.ast,
                &mut self.ast_temp,
                inter_state,
            );

            scope_builder!(self, source)
                .collect_items(self.ast.elements());

            self.build_types(source, &mut ty_buffer);

            bound_verifier!(self).verify();            

            self.build_funcs(source, &mut func_buffer);

            self.scope.clear();
        }

        for _ in self.ty_graph.len()..self.types.len() {
            self.ty_graph.close_node();
        }
    }

    fn build_repr(&mut self) {
        let ptr_ty = self.object_module.isa().pointer_type();
        repr_builder!(self, ptr_ty).translate(&self.ty_graph);
    }

    fn generate(self) {
        self.incr.save(&self.incr_path).unwrap();

        let binary = self.object_module.finish().emit().unwrap();
        std::fs::write("catalyst.o", &binary).unwrap();

        if self.input.enabled("no-link") {
            return;
        }

        let mut entry = "/entry:".to_string();
        self.entry_id.unwrap().to_ident(&mut &mut entry);

        let status = {
            time_report!("linking");
            cc::windows_registry::find(&self.triple.to_string(), "link.exe")
                .unwrap()
                .arg("catalyst.o")
                .arg("ucrt.lib")
                .arg("/subsystem:console")
                .arg(entry)
                .status()
                .unwrap()
        };

        // linking
        std::fs::remove_file("catalyst.o").unwrap();

        assert!(status.success(), "{status:?}");
    }

    fn log_diagnostics(&self) {
        if self.diagnostics.is_empty() {
            return;
        }
        
        let mut errors = String::new();

        self.diagnostics
            .iter::<AstError>()
            .map(|errs| errs.for_each(|err| 
                err.display(
                    &self.sources, 
                    &mut errors
                ).unwrap()
            ));

        self.diagnostics
            .iter::<ModuleError>()
            .map(|errs| errs.for_each(|err|
                modules::error::display(
                    err, 
                    &self.sources, 
                    &self.item_lexicon, 
                    &self.units, 
                    &mut errors
                ).unwrap()
            ));

        self.diagnostics
            .iter::<TyError>()
            .map(|errs| errs.for_each(|err| 
                typec::error::display(
                    err, 
                    &self.sources, 
                    &self.types, 
                    &self.ty_lists, 
                    &mut errors
                ).unwrap()
            ));

        self.diagnostics
            .iter::<InstError>()
            .map(|errs| errs.for_each(|err| 
                instance::error::display(
                    &err, 
                    &self.types, 
                    &self.ty_lists, 
                    &self.sources, 
                    &mut errors
                ).unwrap()
            ));

        println!("{errors}");
        exit!();
    }

    pub fn compile() {
        time_report!("compilation");

        let mut s = Self::new();

        s.load_modules();
        s.log_diagnostics();
        
        s.build_tir();
        s.log_diagnostics();
        
        s.build_repr();

        s.generate();
    }
}

fn get_exe_modification_time() -> Option<SystemTime> {
    std::fs::metadata(&std::env::current_exe().ok()?).map(|m| m.modified()).flatten().ok()
}

pub enum Subcommand {
    Compile(PathBuf),
    None,
}

const SUBCOMMANDS: &'static str = "c";

impl Subcommand {
    pub fn new(input: &CmdInput) -> Self {
        let Some(subcommand) = input.args().get(1) else {
            println!("usage: catalyst {INFO}<sub>{END} ...");
            println!("options: {INFO}{SUBCOMMANDS}{END}");
            exit!();
        };

        match subcommand.as_str() {
            "c" => {
                let path = input.field("path").unwrap_or(".");
                return Self::Compile(PathBuf::from(path));
            }
            sub => {
                println!("invalid subcommand: {sub}");
                println!("options: {INFO}{SUBCOMMANDS}{END}");
                exit!();
            }
        }
    }

    fn root_path(&self) -> Option<&Path> {
        match self {
            Self::Compile(path) => Some(path),
            _ => None,
        }
    }
}

pub struct TimeReport {
    start: Instant,
    message: String,
}

impl TimeReport {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            start: Instant::now(),
            message: message.into(),
        }
    }
}

impl Drop for TimeReport {
    fn drop(&mut self) {
        let duration = self.start.elapsed();
        println!("{} took {:?}", self.message, duration);
    }
}

/// compiles the catalyst source code, taking command like args as input,
/// complete compilation only works on windows with msvc and you have to invoke
/// compiler inside Native Tools Command Prompt
pub fn _compile() {
    // declare function headers
    
    // for &func in &to_compile {
    //     name_buffer.clear();
        
    //     let ent = &funcs[func];

    //     let Some(linkage) = gen::func_linkage(ent.kind) else {
    //         continue;
    //     };

    //     if ent.flags.contains(TFuncFlags::ENTRY) {
    //         if entry_id.is_some() {
    //             todo!("multiple entry functions");
    //         }
    //         entry_id = Some(ent.id);
    //     }

    //     // just a shortcut
    //     if let Some(incr_func_data) = incr.functions.get_mut(ent.id) {
    //         ent.id.to_ident(&mut name_buffer);
    //         let func_id = module.declare_function(&name_buffer, Linkage::Export, &incr_func_data.signature)
    //             .unwrap();
    //         incr_func_data.temp_id = Some(func_id);
    //         continue;
    //     }

    //     if let TFuncKind::Instance(func, params) = ent.kind {
    //         ReprInstancing {
    //             types: &mut types,
    //             ty_lists: &mut ty_lists,
    //             instances: &mut instances,
    //             sfields: &sfields,
    //             sources: &sources,
    //             repr_fields: &mut repr_fields,
    //             reprs: &mut reprs,
    //             ptr_ty,
    //         }.load_generic_types(
    //             params, 
    //             bodies[func].used_types, 
    //             &mut replace_cache
    //         );
    //     }
        
    //     if linkage == Linkage::Import {
    //         name_buffer.push_str(sources.display(ent.name));
    //     } else {
    //         ent.id.to_ident(&mut name_buffer);
    //     }

    //     instance::func::translate_signature(
    //         &ent.sig,
    //         &mut ctx.func.signature,
    //         &reprs,
    //         &types,
    //         &ty_lists,
    //         &sources,
    //         system_call_convention,
    //         ent.flags.contains(TFuncFlags::ENTRY),
    //     );

    //     // println!("{}", sources.display(ent.name));
    //     // println!("{:?}", ctx.func.signature);

    //     let func_id = module
    //         .declare_function(&name_buffer, linkage, &ctx.func.signature)
    //         .unwrap();

    //     ctx.func.signature.clear(CallConv::Fast);

    //     replace_cache.replace(&mut types, &mut reprs);

    //     func_lookup[func] = PackedOption::from(func_id);
    // }
    

    // // define
    // {
    //     let mut variable_set = EntitySet::new();
    //     let mut stack_slot_lookup = SecondaryMap::new();
    //     let mut mir_to_ir_lookup = SecondaryMap::new();
    //     let mut ir_block_lookup = SecondaryMap::new();
    //     let mut tir_mapping = SecondaryMap::new();
    //     let mut reloc_temp = vec![];
    //     let mut name_buffer = String::new();
    //     let mut def_frontier = vec![];

    //     while let Some(func) = to_compile.pop() {

    //         let ent = &funcs[func];

    //         if gen::func_linkage(ent.kind)
    //             .map(|l| l == Linkage::Import)
    //             .unwrap_or(true)
    //         {
    //             continue;
    //         }

    //         if let Some(func_incr_data) = incr.functions.get_mut(ent.id) && !func_incr_data.defined {
    //             func_incr_data.defined = true;

    //             if let Some(temp_id) = func_incr_data.temp_id {
    //                 func_lookup[func] = PackedOption::from(temp_id);
    //             }

    //             let func_id = func_lookup[func].unwrap();

    //             def_frontier.extend(
    //                 func_incr_data.reloc_records.iter().map(|r| r.name)
    //             );
                
    //             while let Some(id) = def_frontier.pop() {
    //                 let Some(func_incr_data) = incr.functions.get_mut(id) else {
    //                     unreachable!();
    //                 };

    //                 if func_incr_data.defined {
    //                     continue;
    //                 }
    //                 func_incr_data.defined = true;
                    
    //                 todo!()
    //             }

    //             let Some(func_incr_data) = incr.functions.get_mut(ent.id) else {
    //                 unreachable!();
    //             };

    //             reloc_temp.clear();
    //             reloc_temp.extend(func_incr_data.reloc_records
    //                 .iter()
    //                 .map(|r| r.to_mach_reloc(&mut name_buffer, &module))
    //             );

    //             module.define_function_bytes(func_id, &func_incr_data.bytes, &reloc_temp).unwrap();

    //             continue;
    //         }

    //         let func = if let TFuncKind::Instance(func, params) = ent.kind {
    //             ReprInstancing {
    //                 types: &mut types,
    //                 ty_lists: &mut ty_lists,
    //                 instances: &mut instances,
    //                 sfields: &sfields,
    //                 sources: &sources,
    //                 repr_fields: &mut repr_fields,
    //                 reprs: &mut reprs,
    //                 ptr_ty,
    //             }.load_generic_types(
    //                 params, 
    //                 bodies[func].used_types, 
    //                 &mut replace_cache
    //             );
    //             func
    //         } else {
    //             func
    //         };

    //         tir_mapping.clear();
    //         function.clear();
    //         MirBuilder {
    //             system_call_convention,
    //             func_id: func,
    //             reprs: &reprs,
    //             types: &types,
    //             funcs: &mut funcs,
    //             func: &mut function,
    //             tir_mapping: &mut tir_mapping,
    //             body: &bodies[func],
    //             ptr_ty,
    //             return_dest: None,
    //             sources: &sources,
    //             ty_lists: &ty_lists,
    //             func_lists: &tfunc_lists,
    //             sfields: &sfields,
    //             bound_impls: &bound_impls,
    //             repr_fields: &repr_fields,
    //             builtin_types: &builtin_types,
    //             diagnostics: &mut diagnostics,
    //             module: &mut module,
    //             func_instances: &mut func_instances,
    //             func_lookup: &mut func_lookup,
    //         }
    //         .translate_func()
    //         .unwrap();

    //         if !diagnostics.is_empty() {
    //             continue;
    //         }

    //         // println!("{}", MirDisplay::new(&sources, &ty_lists, &function, &types));
    //         ctx.clear();
    //         mir_to_ir_lookup.clear();
    //         let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);
    //         CirBuilder {
    //             module: &mut module,
    //             function_lookup: &mut func_lookup,
    //             builder: &mut builder,
    //             value_lookup: &mut mir_to_ir_lookup,
    //             source: &function,
    //             sources: &sources,
    //             block_lookup: &mut ir_block_lookup,
    //             stack_slot_lookup: &mut stack_slot_lookup,
    //             t_funcs: &funcs,
    //             reprs: &reprs,
    //             variable_set: &mut variable_set,
    //             types: &types,
    //             ty_lists: &ty_lists,
    //         }
    //         .generate();

    //         replace_cache.replace(&mut types, &mut reprs);
    //         // println!("{}", sources.display(ent.name));
    //         // println!("{}", ctx.func.display());
            
    //         let id = func_lookup[func].unwrap();

    //         let mut mem = vec![];
    //         ctx.compile_and_emit(module.isa(), &mut mem).unwrap();
    //         let reloc_records = ctx.mach_compile_result.as_ref().unwrap().buffer.relocs(); 
    //         module.define_function_bytes(id, &mem, reloc_records).unwrap();
            
    //         {
    //             let ent = &funcs[func];
    //             let incr_func_data = IncrFuncData {
    //                 signature: ctx.func.signature.clone(),
    //                 bytes: mem,
    //                 temp_id: Some(id),
    //                 defined: true,
    //                 reloc_records: reloc_records
    //                     .iter()
    //                     .map(|r| IncrRelocRecord::from_mach_reloc(r, &module))
    //                     .collect(),
    //             };
    //             incr.functions.insert(ent.id, incr_func_data);

    //             let home = ent.home_module_id(&ty_lists, &modules, &types);
    //             incr.modules.get_mut(home).unwrap().owned_functions.insert(ent.id, ());
    //         }

    //         stack_slot_lookup.clear();
    //         mir_to_ir_lookup.clear();
    //         ir_block_lookup.clear();
    //         variable_set.clear();
    //     }
    // }
}

#[test]
fn test_parse() {
    let vec = Vec::<i8>::with_capacity(1024);
    let vec2 = vec.clone();

    assert_eq!(vec.capacity(), vec2.capacity());
}