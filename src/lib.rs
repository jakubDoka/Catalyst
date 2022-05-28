#![feature(result_option_inspect)]
#![feature(let_else)]
#![feature(bool_to_option)]
#![feature(let_chains)]
#![feature(result_flattening)]
#![feature(scoped_threads)]

use cli::CmdInput;

use cranelift_codegen::ir::{Type, ExternalName};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::settings::{Flags, Configurable};
use cranelift_codegen::{Context, MachReloc};

use cranelift_frontend::{FunctionBuilderContext, FunctionBuilder};
use cranelift_jit::{JITModule, JITBuilder};
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};

use errors::Diagnostics;
use incr::{Incr, IncrFuncData, IncrRelocRecord};
use instance::*;

use instance::func::{MirBuilderContext, PatternStacks};
use instance::repr::{build_builtin_reprs, build_reprs};
use target_lexicon::Triple;

use modules::*;
use parser::*;
use lexer::*;
use module_types::*;
use instance_types::*;
use storage::*;
use typec_types::*;
use typec::*;
use ast::*;
use gen::*;
use matching::*;

use std::path::PathBuf;
use std::str::FromStr;
use std::time::{Instant, SystemTime};
use std::{path::Path};

/// shorthand for exiting the process
macro_rules! exit {
    ($expr:expr) => {
        std::process::exit($expr);
    };

    () => {
        exit!(1);
    };
}

/// utility for simple scoped benchmarking
macro_rules! time_report {
    ($message:expr) => {
        let _report = TimeReport::new($message);
    };
}

/// Main extremely big object containing all needed state for compilation. Currently, the
/// allocations are accumulated and memory is basically getting freed only at the end of 
/// the program. Compilation is also single threaded. This should change in the future, mainly 
/// for codegen faze which takes most of the time.
/// 
/// TODO: This could get fixed with dependency analysis, that would eliminate useless objects.
/// Question is whether we would compile programs at such scale this would matter.
pub struct Compiler {
    // cmd
    input: CmdInput,
    subcommand: Subcommand,

    // incremental data
    incr: Incr,
    _jit_incr: Incr,
    _jit_incr_path: PathBuf,
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
    ty_graph: typec_types::Graph<Ty>,
    types: Types,
    builtin_types: BuiltinTypes,
    funcs: Funcs,
    ty_lists: TyLists,
    instances: Instances,
    ty_comps: TyComps,
    ty_comp_lookup: TyCompLookup,
    func_lists: TFuncLists,
    bound_impls: BoundImpls,
    func_bodies: FuncBodies,
    tir_temp: FramedStack<Tir>,
    scope_context: ScopeContext,
    tir_temp_body: TirData,
    func_meta: FuncMeta,
    pattern_graph: PatternGraph<Tir, PatternMeta>,

    // jit
    _jit_module: JITModule,
    host_isa: Box<dyn TargetIsa>,
    _jit_compile_results: SparseMap<Func, CompileResult>,

    // generation
    entry_id: Option<ID>,
    object_module: ObjectModule,
    triple: Triple,
    reprs: Reprs,
    repr_fields: ReprFields,
    compile_results: SecondaryMap<Func, CompileResult>,
    signatures: Signatures,
    ty_order: Vec<Ty>,
}

impl Compiler {

    /// Creates an compiler instance. For some reason this process is surprisingly slow.
    fn new() -> Self {
        time_report!("initialization of compiler");

        let input = CmdInput::new();

        let subcommand = Subcommand::new(&input);
        
        let modified_time = get_exe_modification_time();
        let root_path = subcommand.root_path().unwrap();
        let incr_path = root_path.join("incr.bin");
        let jit_incr_path = root_path.join("jit_incr.bin");
        let (incr, jit_incr) = if let Some(modified_time) = modified_time {
            (
                Incr::load(format!("{modified_time:?}"), &incr_path),
                Incr::load(format!("{modified_time:?}"), &jit_incr_path),
            )
        } else {
            (
                Incr::default(),
                Incr::default(),
            )
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

        let mut types = Types::new();
        let builtin_types = BuiltinTypes::new(
            &mut sources, 
            &mut builtin_source, 
            &mut types
        );
        let mut ty_lists = TyLists::new();
        let mut funcs = Funcs::new();
        let mut func_meta = FuncMeta::new();

        let b_source = builtin_source.source;
        typec::create_builtin_items(
            &mut types, 
            &mut ty_lists, 
            &builtin_types, 
            &mut funcs, 
            &mut func_meta,
            &mut sources, 
            &mut builtin_source, 
            &mut modules[b_source].items,
        );

        let (jit_module, host_isa) = Self::init_jit_module(&input);

        let mut reprs = Reprs::new();
        build_builtin_reprs(host_isa.pointer_type(), &mut reprs, &builtin_types);

        let (object_module, triple) = Self::init_object_module(&input);

        Self {            
            subcommand: Subcommand::new(&input),
            input,

            incr,
            _jit_incr: jit_incr,
            _jit_incr_path: jit_incr_path,
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
            
            ty_graph: typec_types::Graph::new(),
            types,
            builtin_types,
            funcs,
            ty_lists,
            instances: Instances::new(),
            ty_comps: TyComps::new(),
            ty_comp_lookup: TyCompLookup::new(),
            func_lists: TFuncLists::new(),
            bound_impls: BoundImpls::new(),
            func_bodies: FuncBodies::new(),
            tir_temp: FramedStack::new(),
            tir_temp_body: TirData::new(),
            scope_context: ScopeContext::new(),
            func_meta,
            pattern_graph: PatternGraph::new(),

            _jit_module: jit_module,
            _jit_compile_results: SparseMap::new(),
            host_isa,
                        
            entry_id: None,
            object_module,
            triple,
            reprs,
            repr_fields: ReprFields::new(),
            compile_results: SecondaryMap::new(),
            signatures: Signatures::new(),
            ty_order: Vec::new(),
        }
    }

    /// All customization of these components should be handled here.
    fn init_jit_module(_input: &CmdInput) -> (JITModule, Box<dyn TargetIsa>) {
        let isa = {
            let setting_builder = cranelift_codegen::settings::builder();

            let flags = Flags::new(setting_builder);
    
            let target_triple = target_lexicon::Triple::host();
    
            cranelift_codegen::isa::lookup(target_triple)
                .unwrap()
                .finish(flags)
                .unwrap()
        };

        let builder = JITBuilder::new(
            cranelift_module::default_libcall_names()
        ).unwrap();

        (JITModule::new(builder), isa)
    }

    /// All target configuration is performed here.
    fn init_object_module(input: &CmdInput) -> (ObjectModule, Triple) {
        let (isa, triple) = {
            let mut setting_builder = cranelift_codegen::settings::builder();

            let verify = input
                .enabled("no-verify")
                .then_some("false")
                .unwrap_or("true");
            
            setting_builder.set("enable_verifier", verify).unwrap();
            
            if let Some(opt_level) = input.field("o") {
                let opt_level = match opt_level {
                    "0" | "none" => "none",
                    "1" | "speed" => "speed",
                    "2" | "speed_and_size" => "speed_and_size",                 
                    _ => {
                        println!("{ERR}error:{END} unknown optimization level: {}", opt_level);
                        println!("{INFO}info:{END} use one of: none(0), speed(1), speed_and_size(2)");
                        exit!(1);
                    }
                };
                setting_builder.set("opt_level", opt_level).unwrap();
            }

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

        (object_module, triple)
    }

    /// All source code is loaded into memory here.
    /// 
    /// TODO: Try to use streams instead of `read_to_string`, Files 
    /// could get loaded while parsing, though performance improvement 
    /// is questionable.
    fn load_modules(&mut self) {
        time_report!("loading of modules");

        let Subcommand::Compile(path) = &self.subcommand else {
            unreachable!();
        };

        let Ok(unit_order) = unit_builder!(self).load_units(&path) else {
            return;
        };

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

    /// every module imports set of builtin constructs, work is done here
    fn load_builtin_scope_items(&mut self, source: Source) {
        for item in self.modules[self.builtin_source.source].items.iter() {
            self.scope
                .insert(&mut self.diagnostics, source, item.id, item.to_scope_item())
                .unwrap();
        }
    }

    /// The module imports are read from temporary ast and minimal data is preserved.
    /// 
    /// TODO: Could there be an option to optimize this?
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

    /// Generic type representation is built here. `ty_buffer` is for memory reuse and should be empty
    /// before and after this function.
    fn build_types(&mut self, stage: usize, source: Source, ty_buffer: &mut Vec<Ty>) {
        ty_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .skip(stage)
                .filter_map(|item| item.kind.may_read::<Ty>()),
        );

        for ty in ty_buffer.drain(..) {
            ty_builder!(self, ty).build();           
        }
    }

    /// Similar to `Self::build_types` but for functions. This action depends on types 
    /// so it has to be called after.
    fn build_funcs(&mut self, stage: usize, source: Source, func_buffer: &mut Vec<Func>) {
        func_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .skip(stage)
                .filter_map(|item| item.kind.may_read::<Func>()),
        );

        for func in func_buffer.drain(..) {
            let id = self.funcs.ents[func].id;
            if self.incr.functions.get(id).is_some() {
                continue;
            }

            self.tir_temp_body.clear();
            if tir_builder!(self, func).build().is_err() {
                continue;
            };
            // println!("{}", TirDisplay::new(
            //     &self.types, 
            //     &self.ty_lists, 
            //     &self.ty_comps, 
            //     &self.sources, 
            //     &self.tir_temp_body, 
            //     self.func_meta[func].body,
            // ));
            self.func_bodies[func] = self.tir_temp_body.clone();
        }
    }

    /// Compute the type layouts. This is only used for incremental 
    /// computation while jit-compiling macros.
    fn build_layouts(&mut self, bottom: usize) {
        let iter = (bottom..self.types.len()).map(|i| {
            let ty = Ty::new(i);
            ty
        });
        
        for ty in iter.clone() {
            self.ty_graph.add_vertex(ty);
        }

        let check_point = self.ty_order.len();
        if let Err(cycle) = self.ty_graph.total_ordering(&mut self.ty_order) {
            self.diagnostics.push(TyError::InfinitelySizedType {
                cycle, 
            });
        }

        layout_builder!(self).build_layouts(&self.ty_order[check_point..]);
        build_reprs(self.host_isa.pointer_type(), &mut self.reprs, iter);
        self.ty_graph.clear();
    }

    /// Probably the slowest stage in frontend. Building Tir means 
    /// type-checking all imported source code. Parsing is also included
    /// so that ast does not have to be accumulated for all files. Types are 
    /// checked one ta the time but Tir is accumulated. Tir is also generic 
    /// and instances are not materialized here but rather the Tir has notion 
    /// of generic calls.
    fn build_tir(&mut self) {
        time_report!("building of tir");

        let mut func_buffer = vec![];
        let mut ty_buffer = vec![];

        for source in self.module_order.clone() { // it really does not matter
            self.load_builtin_scope_items(source);

            self.ast.clear();
            let mut inter_state_opt = Some(Parser::parse_imports(
                &self.sources,
                &mut self.diagnostics, 
                &mut self.ast, 
                &mut self.ast_temp, 
                source
            ));
            
            self.build_scope(source);

            let mut stage = 0;

            while let Some(inter_state) = inter_state_opt { 
                self.ast.clear();
                inter_state_opt = Parser::parse_code_chunk(
                    &self.sources,
                    &mut self.diagnostics,
                    &mut self.ast,
                    &mut self.ast_temp,
                    inter_state,
                );            

                self.log_diagnostics();

                let bottom = self.types.len();

                scope_builder!(self, source)
                    .collect_items(self.ast.elements());

                self.log_diagnostics();
                

                self.build_types(stage, source, &mut ty_buffer);

                self.log_diagnostics();

                bound_verifier!(self).verify();            

                self.build_funcs(stage, source, &mut func_buffer);

                self.build_layouts(bottom);

                stage = self.modules[source].items.len();
            }

            self.scope.clear();
        }
    }

    /// Incr data needs to be converted into a form understood be compiler components. The hashes
    /// are dispatched into entities uniquely allocated by compiler. Point is that the compiler
    /// can allocate function entities differently but ID should never change fi source file haven't been 
    /// changed.
    fn incr_data_to_compile_result(&self, incr_func_data: &IncrFuncData) -> CompileResult {
        let bytes = incr_func_data.bytes.clone();
        let relocs = incr_func_data.reloc_records
            .iter()
            .map(|rec| {
                let func = self.funcs.instances.get(rec.name).unwrap().clone(); 
                MachReloc {
                    offset: rec.offset,
                    srcloc: rec.srcloc,
                    kind: rec.kind,
                    name: ExternalName::user(0, func.as_u32()),
                    addend: rec.addend,
                }
            })
            .collect();
        
        CompileResult {
            bytes,
            relocs,
        }
    }

    /// here we try to skip the function compilation by retrieving it from incremental data.
    /// This includes loading all dependant functions as well.
    fn skip_incrementally(
        &mut self, 
        func: Func, 
        id: ID, 
        frontier: &mut Vec<ID>, 
        reused_incr_funcs: &mut Vec<Func>,
        signatures: &mut Signatures,
    ) -> bool {
        let Some(incr_func_data) = self.incr.functions.get(id) else {
            return false;
        };

        let start = reused_incr_funcs.len();
        
        frontier.extend(incr_func_data.dependencies());

        while let Some(id) = frontier.pop() {
            if let Some(shadow) = self.funcs.instances.insert(id, self.funcs.ents.next_key()) {
                self.funcs.instances.insert(id, shadow).unwrap();
                continue;
            }

            let func_ent = FuncEnt {
                id,
                ..Default::default()
            };

            let func = self.funcs.ents.push(func_ent);
            reused_incr_funcs.push(func);

            let Some(incr_func_data) = self.incr.functions.get(id) else {
                unreachable!();
            };

            signatures.insert(func, incr_func_data.signature.clone());

            frontier.extend(incr_func_data.dependencies());
        }

        self.compile_results[func] = self.incr_data_to_compile_result(incr_func_data);

        for &func in &reused_incr_funcs[start..] {
            let id = self.funcs.ents[func].id;
            let Some(incr_func_data) = self.incr.functions.get(id) else {
                unreachable!();
            };

            self.compile_results[func] = self.incr_data_to_compile_result(incr_func_data);
        }

        true
    }

    /// function swaps all generic types for concrete types, preparing the 
    /// lowering fo functions in specific type context.
    fn load_generic_params(
        &mut self, 
        id: Func, 
        params: TyList, 
        parent: PackedOption<Func>, 
        ptr_ty: Type, 
        replace_cache: &mut ReplaceCache
    ) -> Func {
        let Some(parent) = parent.expand() else {
            return id;
        };
        
        ReprInstancing {
            types: &mut self.types,
            ty_lists: &mut self.ty_lists,
            instances: &mut self.instances,
            ty_comps: &self.ty_comps,
            sources: &self.sources,
            repr_fields: &mut self.repr_fields,
            reprs: &mut self.reprs,
            ptr_ty,
        }.load_generic_types(
            params, 
            self.func_bodies[parent].used_types, 
            replace_cache
        );

        parent
    }

    /// Function takes tir and translates it into bite code. It uses type swapping to 
    /// instantiate functions from generic templates. Types still need to be instantiated 
    /// and allocated.
    /// 
    /// TODO: Make codegen multithreaded. This should not be a problem regarding the setup we already have.
    /// Though this may require cloning type context.
    fn generate(&mut self) {
        time_report!("generating");

        let mut pattern_stacks = PatternStacks::new();
        let mut ctx = Context::new();
        let mut builder_ctx = FunctionBuilderContext::new(); 
        let mut replace_cache = ReplaceCache::new();
        let mut signatures = Signatures::new();
        let mut func_ctx = FuncCtx::new();
        let mut mir_builder_ctx = MirBuilderContext::new();
        let mut cir_builder_ctx = CirBuilderContext::new();
        let mut frontier = vec![];
        let mut reused_incr_funcs = vec![];
        let ptr_ty = self.object_module.isa().pointer_type();
        let system_call_convention = self.object_module.isa().default_call_conv();

        for &(func, _) in &self.funcs.to_compile {
            let call_conv = self.funcs.ents[func].flags.call_conv();
            let sig = self.func_meta[func].sig;
            signatures.insert(func, translate_signature(
                call_conv, 
                self.ty_lists
                    .get(sig.args)
                    .iter()
                    .copied(),
                sig.ret, 
                &self.reprs, 
                &self.types, 
                system_call_convention
            ));
        }

        let mut i = 0;
        while i < self.funcs.to_compile.len() {
            let (id, params) = self.funcs.to_compile[i];
            
            i += 1;
            
            let func_ent = self.funcs.ents[id];

            if func_ent.flags.contains(FuncFlags::ENTRY) {
                if self.entry_id.is_some() {
                    println!("multiple entry points");
                    exit!();
                }
                self.entry_id = Some(func_ent.id);
            }

            if self.skip_incrementally(id, func_ent.id, &mut frontier, &mut reused_incr_funcs, &mut signatures) {
                continue;
            }

            let parent = self.load_generic_params(id, params, func_ent.parent, ptr_ty, &mut replace_cache);
            
            let result = MirBuilder {
                system_call_convention,
                return_dest: None,
                func_id: parent,
                ptr_ty,

                types: &mut self.types,
                ty_lists: &mut self.ty_lists,
                ty_comps: &self.ty_comps,
                sources: &self.sources,
                repr_fields: &self.repr_fields,
                reprs: &self.reprs,
                func_lists: &self.func_lists,
                bound_impls: &self.bound_impls,
                builtin_types: &self.builtin_types,
                funcs: &mut self.funcs,
                func: &mut func_ctx,
                body: &self.func_bodies[parent],
                diagnostics: &mut self.diagnostics,
                ctx: &mut mir_builder_ctx,
                func_meta: &self.func_meta,
                pattern_stacks: &mut pattern_stacks,
            }.translate_func();
            
            if result.is_err() {
                continue;
            }
            
            // println!("{}", MirDisplay::new(&self.sources, &self.ty_lists, &func_ctx, &self.types));

            ctx.func.signature = signatures.get(id).unwrap().clone();

            let builder = &mut FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

            CirBuilder {
                builder,
                ctx: &mut cir_builder_ctx,
                signatures: &mut signatures,
                source: &mut func_ctx,
                
                isa: self.object_module.isa(),
                funcs: &self.funcs,
                reprs: &self.reprs,
                types: &self.types,
                builtin_types: &self.builtin_types,
                ty_lists: &self.ty_lists,
                sources: &self.sources,
                func_meta: &self.func_meta,
            }.generate();

            // println!("{}", self.sources.display(self.func_meta[parent].name));
            // println!("{}", ctx.func.display());

            let mut bytes = vec![];
            ctx.compile_and_emit(self.object_module.isa(), &mut bytes)
                .unwrap();
            let relocs = ctx.mach_compile_result.as_ref().unwrap().buffer.relocs().to_vec();

            let compile_result = CompileResult {
                bytes,
                relocs,
            };

            self.compile_results[id] = compile_result;
            ctx.clear();
        }

        self.funcs.to_compile.extend(
            reused_incr_funcs.into_iter().map(|id| (id, TyList::reserved_value()))
        );

        self.signatures = signatures;
    }

    /// Only reachable functions are being included in final executable. 
    /// Which functions are used is determined here. First vec contains 
    /// functions that have local byte-code, second contains imports that 
    /// should only be imported.
    fn collect_used_funcs(&mut self) -> (Vec<Func>, Vec<Func>) {
        time_report!("dead code elimination");

        let mut seen = EntitySet::with_capacity(self.funcs.ents.len());
        let mut frontier = Vec::with_capacity(self.funcs.to_compile.len());

        if let Some(entry_id) = self.entry_id {
            let &entry_func = self.funcs.instances.get(entry_id).unwrap();
            frontier.push(entry_func)
        }
        
        let mut i = 0;
        while let Some(&func) = frontier.get(i) {
            for reloc in &self.compile_results[func].relocs {
                let ExternalName::User { namespace, index } = reloc.name else {
                    unreachable!();
                };

                assert!(namespace == 0);

                let func = Func(index);

                if !seen.insert(func) {
                    continue;
                }
                
                frontier.push(func);
            }

            i += 1;
        }

        let mut to_link = Vec::with_capacity(frontier.len());

        frontier.retain(|&f| {
            if self.funcs.ents[f].flags.contains(FuncFlags::EXTERNAL) {
                to_link.push(f);
                return false;
            }
            true
        });

        (frontier, to_link)
    }

    /// Object file is being created here. Only reachable functions are included.
    fn build_object(&mut self) {
        time_report!("building object file");

        let mut func_lookup = SecondaryMap::new();
        func_lookup.resize(self.funcs.ents.len());
        let system_call_conv = self.object_module.isa().default_call_conv();

        let (to_compile, to_link) = self.collect_used_funcs();

        for func in to_link {
            let call_conv = self.funcs.ents[func].flags.call_conv();
            let sig = self.func_meta[func].sig;
            let name = self.sources.display(self.func_meta[func].name);
            let signature = translate_signature(
                call_conv, 
                self.ty_lists
                    .get(sig.args)
                    .iter()
                    .copied(), 
                sig.ret, 
                &self.reprs, 
                &self.types, 
                system_call_conv,
            );
            let func_id = self.object_module.declare_function(name, Linkage::Import, &signature)
                .unwrap();
            func_lookup[func] = PackedOption::from(func_id);
        }

        let mut name = String::new();
        for &func in &to_compile {
            {
                let id = self.funcs.ents[func].id;
                name.clear();
                id.to_ident(&mut name);
            }
            let signature = self.signatures.get(func).unwrap();
            let linkage = Linkage::Export;
            let func_id = self.object_module.declare_function(&name, linkage, signature)
                .unwrap();
            func_lookup[func] = PackedOption::from(func_id);
        }

        let mut reloc_temp = vec![];
        for func in to_compile {
            let compile_result = &self.compile_results[func];
            reloc_temp.clear();
            reloc_temp.extend(compile_result.relocs.iter().cloned().map(|mut r| {
                let ExternalName::User {
                    namespace,
                    index,
                } = r.name else {
                    unreachable!();
                };
                
                assert!(namespace == 0);

                let func_id = func_lookup[Func(index)].unwrap();

                let name = ExternalName::user(0, func_id.as_u32());

                r.name = name;

                r
            }));

            self.object_module.define_function_bytes(
                func_lookup[func].unwrap(), 
                &compile_result.bytes, 
                &reloc_temp
            ).unwrap();
        }
    }

    /// All compiled functions are saved. That means their byte-code 
    /// signature and relocs. Singular optimally sized file is produced.
    fn save_incr_data(&mut self) {
        time_report!("saving incremental data");

        self.incr.functions.clear();
        
        let mut temp_relocs = vec![];
        for &(func, _) in &self.funcs.to_compile {
            let compile_result = std::mem::take(&mut self.compile_results[func]);

            temp_relocs.clear();
            temp_relocs.extend(
                compile_result.relocs.iter().map(|r| {
                    let ExternalName::User {
                        namespace,
                        index,
                    } = r.name else {
                        unreachable!();
                    };

                    assert!(namespace == 0);

                    let name = self.funcs.ents[Func(index)].id;
                    IncrRelocRecord {
                        offset: r.offset,
                        srcloc: r.srcloc,
                        kind: r.kind,
                        name,
                        addend: r.addend,
                    }
                })
            );

            let signature = self.signatures.remove(func).unwrap();

            let incr_func_data = IncrFuncData {
                signature,
                temp_id: None,
                defined: false,
                bytes: compile_result.bytes,
                reloc_records: temp_relocs.clone(),
            };

            let id = self.funcs.ents[func].id;
            self.incr.functions.insert(id, incr_func_data);
        }

        self.incr.save(&self.incr_path).unwrap();
    }

    /// Linking is last and destructing stage of compilation.
    /// It also takes a tall as external linker needs to be booted.
    /// Currently, only MSVC linker is supported.
    fn link(self) {
        time_report!("linking");

        let binary = self.object_module.finish().emit().unwrap();
        std::fs::write("catalyst.o", &binary).unwrap();

        if self.input.enabled("no-link") {
            return;
        }

        let mut entry = "/entry:".to_string();
        let Some(entry_id) = self.entry_id else {
            println!("{ERR}|> program is missing entry point{END}");
            exit!();
        };
        
        entry_id.to_ident(&mut &mut entry);

        let output = cc::windows_registry::find(&self.triple.to_string(), "link.exe")
            .unwrap()
            .arg("catalyst.o")
            .arg("ucrt.lib")
            .arg("/subsystem:console")
            .arg(entry)
            .output()
            .unwrap();

        // linking
        std::fs::remove_file("catalyst.o").unwrap();

        assert!(output.status.success(), "{:?}", output.status);
    }

    /// Function iterates trough all possible diagnostic options, 
    /// logs them and if errors are encountered, exits.
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

    /// All stages glued together perform compilation. Calling this is preferable, but
    /// stages of compilation may be exposed as public API.
    pub fn compile() {
        time_report!("compilation");

        let mut s = Self::new();

        s.load_modules();
        s.log_diagnostics();

        s.incr.reduce(&s.module_map, &s.modules, &s.module_order);
        
        s.build_tir();
        s.log_diagnostics();
        
        s.generate();
        s.log_diagnostics();
        
        
        s.build_object();
        
        s.save_incr_data();

        s.link();
    }
}

/// We need to know if compiler is sync with incremental data. 
/// This si simplest most optimal solution that plays well with 
/// development cycle. 
fn get_exe_modification_time() -> Option<SystemTime> {
    std::fs::metadata(&std::env::current_exe().ok()?)
        .map(|m| m.modified())
        .flatten()
        .ok()
}

/// Sub command handles parsing of command line arguments
/// for all subcommands.
pub enum Subcommand {
    Compile(PathBuf),
    None,
}

const SUBCOMMANDS: &'static str = "c";

impl Subcommand {
    /// Builds a subcommand based of command line input.
    pub fn new(input: &CmdInput) -> Self {
        let Some(subcommand) = input.args().get(0) else {
            println!("usage: catalyst {INFO}<sub>{END} ...");
            println!("options: {INFO}{SUBCOMMANDS}{END}");
            exit!();
        };

        match subcommand.as_str() {
            "c" => {
                let path = input.args().get(1).map(String::as_str).unwrap_or(".");
                return Self::Compile(PathBuf::from(path));
            }
            sub => {
                println!("invalid subcommand: {sub}");
                println!("options: {INFO}{SUBCOMMANDS}{END}");
                exit!();
            }
        }
    }

    /// Returns a path to the input project.
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
        println!("{WEAK}{}:{END} {:?}", self.message, duration);
    }
}

#[derive(Default, Clone)]
struct CompileResult {
    bytes: Vec<u8>,
    relocs: Vec<MachReloc>,
}