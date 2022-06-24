#![feature(result_option_inspect)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(result_flattening)]
#![feature(scoped_threads)]

pub mod dead_code_elim;
pub mod generator;
pub mod logger;
pub mod scope_builder;
pub mod source_loader;
pub mod state;
pub mod subcommand;
pub mod tir_builder;

pub use generator::GenerationContext;
pub use state::{DeadCodeElim, Generator, Logger, MainScopeBuilder, MainTirBuilder, SourceLoader};
pub use subcommand::Subcommand;

use cli::CmdInput;

use cranelift_codegen::ir::{ExternalName, Signature, Type};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_codegen::{Context, MachReloc};

use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use ast::*;
use errors::*;
use gen::*;
use incr::*;
use instance::*;
use instance_types::*;
use lexer::*;
use matching::*;
use module_types::*;
use modules::*;
use ownership::*;
use parser::*;
use storage::*;
use typec::*;
use typec_types::*;

use std::path::PathBuf;
use std::str::FromStr;
use std::time::{Instant, SystemTime};

pub const CATALYST_ENTRY: &str = "__catalyst_entry__";

pub type TyOrder = Vec<Ty>;
pub type CompileResults = SecondaryMap<Func, CompileResult>;

/// shorthand for exiting the process
#[macro_export]
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
    // pool
    vec_pool: VecPool,

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
    ast_data: AstData,
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

    // typec
    ty_graph: TyGraph,
    types: Types,
    builtin_types: BuiltinTypes,
    funcs: Funcs,
    ty_lists: TyLists,
    ty_instances: TyInstances,
    ty_comps: TyComps,
    func_lists: FuncLists,
    bound_impls: BoundImpls,
    tir_stack: TirStack,
    scope_context: ScopeContext,
    tir_data: TirData,
    tir_pattern_graph: TirPatternGraph,
    to_compile: ToCompile,
    func_instances: FuncInstances,
    to_link: ToLink,
    macros: Macros,

    // ownership
    o_ctx: OwnershipContext,

    // globals
    globals: Globals,
    _global_data: GlobalData,
    global_map: GlobalMap,

    // jit
    _jit_module: JITModule,
    host_isa: Box<dyn TargetIsa>,
    _jit_compile_results: SparseMap<Func, CompileResult>,

    // generation
    object_module: ObjectModule,
    triple: Triple,
    reprs: Reprs,
    repr_fields: ReprFields,
    compile_results: SecondaryMap<Func, CompileResult>,
    signatures: Signatures,
    ty_order: TyOrder,
    mir_builder_context: MirBuilderContext,
    cir_builder_context: CirBuilderContext,
    context: Context,
    func_ctx: FuncCtx,
    generation_context: GenerationContext,
    initializers: Initializers,
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
        let incr = if let Some(modified_time) = modified_time {
            Incr::load(format!("{modified_time:?}"), &incr_path)
        } else {
            Incr::default()
        };

        let mut sources = Sources::new();
        let mut builtin_source = BuiltinSource::new(&mut sources);

        let mut types = Types::new();
        let builtin_types = BuiltinTypes::new(&mut sources, &mut builtin_source, &mut types);

        let (jit_module, host_isa) = Self::init_jit_module(&input);

        let mut reprs = Reprs::new();
        build_builtin_reprs(host_isa.pointer_type(), &mut reprs, &builtin_types);

        let (object_module, triple) = Self::init_object_module(&input);

        Self {
            vec_pool: VecPool::new(),

            subcommand: Subcommand::new(&input),
            input,

            incr,
            incr_path,

            sources,
            builtin_source,

            ast_data: AstData::new(),
            ast_temp: FramedStack::new(),

            scope: Scope::new(),
            module_map: Map::new(),
            loader_context: LoaderContext::new(),
            modules: Modules::new(),
            units: Units::new(),
            module_order: Vec::new(),

            diagnostics: Diagnostics::new(),

            ty_graph: TyGraph::new(),
            types,
            builtin_types,
            funcs: Funcs::new(),
            ty_lists: TyLists::new(),
            ty_instances: TyInstances::new(),
            ty_comps: TyComps::new(),
            func_lists: FuncLists::new(),
            bound_impls: BoundImpls::new(),
            tir_stack: FramedStack::new(),
            tir_data: TirData::new(),
            scope_context: ScopeContext::new(),
            tir_pattern_graph: PatternGraph::new(),
            to_compile: ToCompile::new(),
            func_instances: FuncInstances::new(),
            to_link: ToLink::new(),
            macros: Macros::new(),

            o_ctx: OwnershipContext::new(),

            globals: Globals::new(),
            _global_data: GlobalData::new(),
            global_map: GlobalMap::new(),

            _jit_module: jit_module,
            _jit_compile_results: SparseMap::new(),
            host_isa,

            object_module,
            triple,
            reprs,
            repr_fields: ReprFields::new(),
            compile_results: SecondaryMap::new(),
            signatures: Signatures::new(),
            ty_order: Vec::new(),
            mir_builder_context: MirBuilderContext::new(),
            cir_builder_context: CirBuilderContext::new(),
            context: Context::new(),
            func_ctx: FuncCtx::new(),
            generation_context: GenerationContext::new(),
            initializers: Initializers::new(),
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

        let builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();

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
                        println!(
                            "{INFO}info:{END} use one of: none(0), speed(1), speed_and_size(2)"
                        );
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
            let builder =
                ObjectBuilder::new(isa, "catalyst", cranelift_module::default_libcall_names())
                    .unwrap();
            ObjectModule::new(builder)
        };

        (object_module, triple)
    }

    fn init_builtin(&mut self) {
        let b_source = self.builtin_source.source;
        builtin_builder!(self).create_builtin_items(&mut self.modules[b_source].items)
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

    /// Probably the slowest stage in frontend. Building Tir means
    /// type-checking all imported source code. Parsing is also included
    /// so that ast does not have to be accumulated for all files. Types are
    /// checked one ta the time but Tir is accumulated. Tir is also generic
    /// and ty_instances are not materialized here but rather the Tir has notion
    /// of generic calls.
    fn build_tir(&mut self) {
        time_report!("building of tir");
        main_tir_builder!(self).build(&self.module_order);
    }

    /// Function takes tir and translates it into bite code. It uses type swapping to
    /// instantiate functions from generic templates. Types still need to be instantiated
    /// and allocated.
    ///
    /// TODO: Make codegen multithreaded. This should not be a problem regarding the setup we already have.
    /// Though this may require cloning type context.
    fn generate(&mut self) {
        time_report!("generating");
        generator!(
            self,
            self.incr.modules,
            self.incr.functions,
            *self.object_module.isa()
        )
        .generate();
    }

    /// Only reachable functions are being included in final executable.
    /// Which functions are used is determined here. First vec contains
    /// functions that have local byte-code, second contains imports that
    /// should only be imported.
    fn collect_used_funcs(&mut self) -> (Vec<Global>, Vec<Func>, Vec<Func>) {
        time_report!("dead code elimination");

        let mut seen_funcs = EntitySet::with_capacity(self.funcs.len());
        let mut seen_globals = EntitySet::with_capacity(self.globals.len());
        let mut frontier = Vec::with_capacity(self.to_compile.len());
        let mut globals = Vec::with_capacity(self.globals.len());

        frontier.extend(self.initializers.iter().map(|(id, _)| id));

        // explanted after while loop
        self.initializers.reverse();

        let mut i = 0;
        while let Some(&func) = frontier.get(i) {
            for reloc in &self.compile_results[func].relocs {
                let ExternalName::User { namespace, index } = reloc.name else {
                    unreachable!();
                };

                match namespace {
                    FUNC_NAMESPACE => {
                        let func = Func(index);

                        if !seen_funcs.insert(func) {
                            continue;
                        }

                        frontier.push(func);
                    }
                    DATA_NAMESPACE => {
                        let global = Global(index);

                        if !seen_globals.insert(global) {
                            continue;
                        }

                        globals.push(global);
                        frontier.push(self.globals[global].init);
                        self.initializers
                            .push((self.globals[global].init, Some(global).into()));
                    }
                    _ => unreachable!(),
                }
            }

            i += 1;
        }

        // firs we reverse the initializers and then we push global initializers
        // and then vi reverse again. This ensures ordering where all globals are
        // initialized be before entrypoint functions and entrypoint functions are
        // also preserve the correct order.
        self.initializers.reverse();

        let mut to_link = Vec::with_capacity(frontier.len());

        frontier.retain(|&f| {
            if self.funcs[f].flags.contains(FuncFlags::EXTERNAL) {
                to_link.push(f);
                return false;
            }
            true
        });

        (globals, frontier, to_link)
    }

    /// Object file is being created here. Only reachable functions are included.
    fn build_object(&mut self) {
        time_report!("building object file");

        let mut func_lookup = SecondaryMap::new();
        let mut global_lookup = SecondaryMap::new();
        let mut data_ctx = DataContext::new();
        func_lookup.resize(self.funcs.len());
        global_lookup.resize(self.globals.len());
        let system_call_conv = self.object_module.isa().default_call_conv();

        let (globals, mut to_compile, to_link) = self.collect_used_funcs();
        let mut name = String::new();

        for global in globals {
            let global_ent = self.globals[global];
            {
                let id = global_ent.id;
                name.clear();
                id.to_ident(&mut name);
            }
            let global_ref = self
                .object_module
                .declare_data(&name, Linkage::Export, true, false)
                .unwrap();
            global_lookup[global] = PackedOption::from(global_ref);

            if let Some(_data) = global_ent.bytes.expand() {
                todo!();
            } else {
                let arch32 = self.object_module.isa().pointer_bytes() == 4;
                let size = self.reprs[global_ent.ty].layout.size().arch(arch32);
                data_ctx.define_zeroinit(size as usize);
            }

            self.object_module
                .define_data(global_ref, &data_ctx)
                .unwrap();
            data_ctx.clear();
        }

        for func in to_link {
            let call_conv = self.funcs[func.meta()].sig.cc;
            let sig = self.funcs[func.meta()].sig;
            let name = self.sources.display(self.funcs[func.meta()].name);
            let signature = translate_signature(
                call_conv,
                self.ty_comps.get(sig.args).iter().map(|arg| arg.ty),
                sig.ret,
                &self.reprs,
                &self.types,
                system_call_conv,
            );
            let func_id = self
                .object_module
                .declare_function(name, Linkage::Import, &signature)
                .unwrap();
            func_lookup[func] = PackedOption::from(func_id);
        }

        for &func in &to_compile {
            {
                let id = self.funcs[func].id;
                name.clear();
                id.to_ident(&mut name);
            }
            let signature = self.signatures.get(func).unwrap();
            let linkage = Linkage::Export;
            let func_id = self
                .object_module
                .declare_function(&name, linkage, signature)
                .unwrap();
            func_lookup[func] = PackedOption::from(func_id);
        }

        // declare entrypoint
        {
            let entry_point = self
                .object_module
                .declare_function(
                    CATALYST_ENTRY,
                    Linkage::Export,
                    &Signature::new(self.object_module.isa().default_call_conv()),
                )
                .unwrap();

            let func = self.funcs.push(Default::default(), Default::default());

            func_lookup[func] = PackedOption::from(entry_point);

            {
                self.func_ctx.clear();

                let entry = self.func_ctx.create_block();
                self.func_ctx.select_block(entry);

                for &(func, global) in self.initializers.iter() {
                    if let Some(global) = global.expand() {
                        let ty = self.globals[global].ty;
                        let access = {
                            let value = self
                                .func_ctx
                                .values
                                .push(ValueEnt::new(ty).flags(MirFlags::POINTER));
                            let kind = InstKind::GlobalAccess(global);
                            let ent = InstEnt::new(kind).value(value);
                            self.func_ctx.add_inst(ent);
                            value
                        };

                        let has_sret = self.reprs[ty].flags.contains(ReprFlags::ON_STACK);
                        if has_sret {
                            let return_value = self.func_ctx.values.push(ValueEnt::new(ty));
                            let args = self.func_ctx.value_slices.push(&[access]);
                            let kind = InstKind::Call(func, args);
                            let ent = InstEnt::new(kind).value(return_value);
                            self.func_ctx.add_inst(ent);
                        } else {
                            let return_value = self.func_ctx.values.push(ValueEnt::new(ty));
                            let kind = InstKind::Call(func, ValueList::reserved_value());
                            let ent = InstEnt::new(kind).value(return_value);
                            self.func_ctx.add_inst(ent);
                            {
                                let kind = InstKind::Assign(return_value);
                                let ent = InstEnt::new(kind).value(access);
                                self.func_ctx.add_inst(ent);
                            }
                        }
                    } else {
                        let return_value = self
                            .func_ctx
                            .values
                            .push(ValueEnt::new(self.builtin_types.int));
                        let kind = InstKind::Call(func, ValueList::reserved_value());
                        let ent = InstEnt::new(kind).value(return_value);
                        self.func_ctx.add_inst(ent);
                    }
                }

                self.func_ctx.add_inst(InstEnt::new(InstKind::Return));
            }

            self.context.func.signature =
                Signature::new(self.object_module.isa().default_call_conv());

            generator!(
                self,
                self.incr.modules,
                self.incr.functions,
                *self.object_module.isa()
            )
            .build_cir_and_emit(func, false);

            to_compile.push(func);
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

                let id = match namespace {
                    FUNC_NAMESPACE => func_lookup[Func(index)].unwrap().as_u32(),
                    DATA_NAMESPACE => global_lookup[Global(index)].unwrap().as_u32(),
                    _ => unreachable!(),
                };

                let name = ExternalName::user(namespace, id);

                r.name = name;

                r
            }));

            self.object_module
                .define_function_bytes(
                    func_lookup[func].unwrap(),
                    &compile_result.bytes,
                    &reloc_temp,
                )
                .unwrap();
        }
    }

    /// All compiled functions are saved. That means their byte-code
    /// signature and relocs. Singular optimally sized file is produced.
    fn save_incr_data(&mut self) {
        time_report!("saving incremental data");
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

        let entry = format!("/entry:{CATALYST_ENTRY}");

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

        assert!(
            output.status.success(),
            "{:?}\nstdout: {}\nstderr: {}",
            output.status,
            String::from_utf8(output.stdout).unwrap(),
            String::from_utf8(output.stderr).unwrap(),
        );
    }

    /// Function iterates trough all possible diagnostic options,
    /// logs them and if errors are encountered, exits.
    fn log_diagnostics(&self) {
        logger!(self).log();
    }

    /// All stages glued together perform compilation. Calling this is preferable, but
    /// stages of compilation may be exposed as public API.
    pub fn compile() {
        time_report!("compilation");

        let mut s = Self::new();

        s.init_builtin();

        s.load_modules();
        s.log_diagnostics();

        s.incr.reduce(&s.modules, &s.module_order);

        s.build_tir();
        s.log_diagnostics();

        s.generate();
        s.log_diagnostics();

        s.build_object();

        s.save_incr_data();

        s.link();
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
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
pub struct CompileResult {
    pub bytes: Vec<u8>,
    pub relocs: Vec<MachReloc>,
}
