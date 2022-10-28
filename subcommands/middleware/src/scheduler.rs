use std::{
    collections::VecDeque,
    default::default,
    iter, mem,
    path::*,
    sync::mpsc::{self, Receiver, Sender, SyncSender},
    thread,
};

use cranelift_codegen::Context;
use cranelift_frontend::FunctionBuilderContext;
use diags::*;
use gen::*;
use lexing::*;
use mir::*;
use mir_t::*;
use packaging::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;
use typec::*;
use typec_t::*;

use crate::{incremental::SweepProjections, *};

macro_rules! sweep {
    ($self:expr, $source:expr => $target:expr) => {
        IncrementalBorrow {
            typec: &mut $source.typec,
            mir: &mut $source.mir,
            gen: &mut $source.gen,
        }
        .sweep(
            &$self.sweep_bitset,
            &mut SweepCtx {
                temp: IncrementalBorrow {
                    typec: &mut $target.typec,
                    mir: &mut $target.mir,
                    gen: &mut $target.gen,
                },
                projections: &mut $self.sweep_ctx,
            },
            &mut InternerTransfer::new(&mut $source.interner, &mut $target.interner),
        );
    };
}

#[derive(Default)]
pub struct Middleware {
    pub workspace: Workspace,
    pub resources: Resources,
    pub package_graph: PackageGraph,
    pub resource_loading_ctx: PackageLoaderCtx,
    pub incremental: Incremental,
    pub temp_incremental: Incremental,
    pub sweep_ctx: SweepProjections,
    pub sweep_bitset: BitSet,
    pub task_graph: TaskGraph,
    pub entry_points: Vec<VRef<Func>>,
}

impl Middleware {
    pub fn new() -> Self {
        Self::default()
    }

    fn reload_resources(&mut self, path: &Path) {
        PackageLoader::new(
            &mut self.resources,
            &mut self.workspace,
            &mut self.incremental.interner,
            &mut self.package_graph,
        )
        .reload(path, &mut self.resource_loading_ctx);
    }

    pub fn update(&mut self, args: &SchedulerArgs) {
        if let Some(ref path) = args.incremental_path {
            self.prepare_incremental(path, &args.path);
        }

        if self.workspace.has_errors() {
            return;
        }

        let (sender, receiver) = mpsc::channel();
        let (gen_sender, _gen_receiver) = mpsc::channel();
        let num_workers = thread::available_parallelism()
            .map_or(1, |n| n.get())
            .min(args.max_cores.unwrap_or(usize::MAX));

        if num_workers == 1 {
            todo!();
        }

        let (workers, _gen_receivers): (Vec<_>, Vec<_>) =
            iter::repeat_with(|| Worker::new(sender.clone(), gen_sender.clone()))
                .take(num_workers)
                .unzip();

        self.build_task_graph();

        let resources = mem::take(&mut self.resources);

        thread::scope(|s| {
            let senders = workers
                .into_iter()
                .map(|(worker, sender)| {
                    let shared = Shared {
                        resources: &resources,
                        jit_isa: &args.jit_isa,
                    };
                    worker.run(s, shared);
                    (sender, Some(Task::default()))
                })
                .collect::<Vec<_>>();

            let tasks = self.expand(receiver, senders, &resources);
        });

        self.resources = resources;
    }

    pub fn expand(
        &mut self,
        receiver: Receiver<Task>,
        mut senders: Vec<(SyncSender<Task>, Option<Task>)>,
        resources: &Resources,
    ) -> Vec<Task> {
        while self.task_graph.has_tasks() {
            for (sender, task) in senders.iter_mut() {
                if let Some(mut task) = task.take() {
                    let Some(package) = self.task_graph.next() else {
                        break;
                    };

                    task.clear();
                    task.modules_to_compile.extend(
                        resources
                            .module_order
                            .iter()
                            .filter(|&&module| resources.modules[module].package == package),
                    );

                    self.sweep_bitset.clear();
                    resources.mark_subgraph(&task.modules_to_compile, &mut self.sweep_bitset);

                    sweep!(self, self.incremental => task);
                    sender.send(task).expect("whoops");
                }
            }

            let mut task = receiver.recv().expect("very bad");
            self.entry_points.append(&mut task.entry_points);
            self.sweep_bitset.clear();
            for &module in &task.modules_to_compile {
                self.sweep_bitset.insert(module.index());
            }
            sweep!(self, task => self.incremental);
        }

        senders
            .into_iter()
            .map(|(_sender, task)| task.unwrap_or_else(|| receiver.recv().expect("we are doomed")))
            .collect::<Vec<_>>()
    }

    pub fn transfer_all(task: &mut Task, incremental: &Incremental) {
        task.typec.transfer(&incremental.typec);
        task.interner.clone_from(&incremental.interner);
        task.mir.bodies.clone_from(&incremental.mir.bodies);
        task.gen
            .compiled_funcs
            .clone_from(&incremental.gen.compiled_funcs);
    }

    pub fn prepare_incremental(&mut self, path: &Path, resource_path: &Path) {
        if let Err(err) = self.load_incremental(path) {
            self.workspace.push(snippet! {
                warn: ("Failed to load incremental data: {}", err);
                info: "Rebuilding from scratch";
                info: ("queried path: {}", path.display());
            })
        };

        self.resources.sources = mem::take(&mut self.incremental.sources);

        self.reload_resources(resource_path);
        self.sweep_bitset.clear();
        for (key, module) in self.resources.modules.iter() {
            if !self.resources.sources[module.source].changed {
                self.sweep_bitset.insert(key.index());
            }
        }

        sweep!(self, self.incremental => self.temp_incremental);
        mem::swap(&mut self.incremental, &mut self.temp_incremental);
        self.temp_incremental.clear();
        self.incremental.loaded = true;
    }

    fn load_incremental(&mut self, path: &Path) -> std::io::Result<()> {
        if self.incremental.loaded || !path.exists() {
            return Ok(());
        }

        let file = std::fs::File::open(path)?;
        let mut buffered = std::io::BufReader::new(file);
        self.incremental = rmp_serde::from_read(&mut buffered)
            .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;

        Ok(())
    }

    pub fn save(&mut self, path: &Path) -> std::io::Result<()> {
        let file = std::fs::File::create(path)?;
        let mut buffered = std::io::BufWriter::new(file);
        self.incremental.sources = mem::take(&mut self.resources.sources);
        rmp_serde::encode::write(&mut buffered, &self.incremental)
            .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;

        Ok(())
    }

    pub fn build_task_graph(&mut self) {
        self.task_graph.clear();

        let mut edges = self
            .resources
            .packages
            .keys()
            .flat_map(|key| {
                self.resources.package_deps[self.resources.packages[key].deps]
                    .iter()
                    .map(move |dep| (dep.ptr, key))
            })
            .collect::<BumpVec<_>>();
        edges.sort_unstable();

        for package_deps in edges.group_by(|(a, _), (b, _)| *a == *b) {
            let package = package_deps[0].0;

            while package.index() >= self.task_graph.meta.len() {
                self.task_graph.meta.push(default());
            }

            let count = self.resources.package_deps[self.resources.packages[package].deps].len();
            let children = self
                .task_graph
                .inverse_graph
                .bump(package_deps.iter().map(|&(_, dep)| dep));

            self.task_graph.meta.push((count, children));

            if count == 0 {
                self.task_graph.frontier.push_back(package);
            }
        }
    }
}

#[derive(Default)]
pub struct TaskGraph {
    meta: Vec<(usize, VRefSlice<Package>)>,
    frontier: VecDeque<VRef<Package>>,
    inverse_graph: BumpMap<VRef<Package>>,
}

impl TaskGraph {
    pub fn next(&mut self) -> Option<VRef<Package>> {
        let current = self.frontier.pop_front();

        if let Some(current) = current {
            let (.., deps) = self.meta[current.index()];
            for &dep in &self.inverse_graph[deps] {
                let index = self.meta[dep.index()].0;
                self.meta[index].0 -= 1;
                if self.meta[index].0 == 0 {
                    self.frontier.push_back(dep);
                }
            }
        }

        current
    }

    pub fn clear(&mut self) {
        self.meta.clear();
        self.frontier.clear();
        self.inverse_graph.clear();
    }

    fn has_tasks(&self) -> bool {
        !self.frontier.is_empty()
    }
}

#[derive(Clone, Copy)]
pub struct Shared<'a> {
    resources: &'a Resources,
    jit_isa: &'a Isa,
}

pub struct SchedulerArgs {
    pub path: PathBuf,
    pub jit_isa: Isa,
    pub incremental_path: Option<PathBuf>,
    pub max_cores: Option<usize>,
}

pub struct Worker {
    pub tasks: Receiver<Task>,
    pub products: Sender<Task>,
    pub gen_tasks: Receiver<GenTask>,
    pub gen_products: Sender<GenTask>,
    pub state: WorkerState,
    pub context: Context,
    pub function_builder_ctx: FunctionBuilderContext,
}

impl Worker {
    pub fn new(
        products: Sender<Task>,
        gen_products: Sender<GenTask>,
    ) -> ((Self, SyncSender<Task>), Sender<GenTask>) {
        let (tx, rx) = mpsc::sync_channel(0);
        let (gen_tx, gen_rx) = mpsc::channel();
        (
            (
                Self {
                    tasks: rx,
                    gen_tasks: gen_rx,
                    products,
                    gen_products,
                    state: WorkerState::default(),
                    context: Context::new(),
                    function_builder_ctx: FunctionBuilderContext::new(),
                },
                tx,
            ),
            gen_tx,
        )
    }

    pub fn run<'a: 'b, 'b>(mut self, thread_scope: &'a thread::Scope<'b, '_>, shared: Shared<'b>) {
        thread_scope.spawn(move || {
            let mut arena = Arena::new();
            let mut jit_ctx = JitContext::new(iter::empty());
            self.state.jit_layouts.ptr_ty = shared.jit_isa.pointer_ty;
            loop {
                let Ok(mut task) = self.tasks.recv() else {
                    break;
                };

                let modules = mem::take(&mut task.modules_to_compile);
                for &module in modules.iter() {
                    self.compile_module(module, &mut arena, &mut task, &mut jit_ctx, &shared);
                }
                task.modules_to_compile = modules;
                if self.products.send(task).is_err() {
                    break;
                }
            }
        });
    }

    fn compile_module(
        &mut self,
        module: VRef<Module>,
        arena: &mut Arena,
        task: &mut Task,
        jit_ctx: &mut JitContext,
        shared: &Shared,
    ) {
        let source = shared.resources.modules[module].source;

        self.parse::<UseAstSkip>(source, arena, task, None, shared);

        let mut macros = typec::build_scope(
            module,
            &mut self.state.scope,
            shared.resources,
            &task.typec,
            &mut task.interner,
        );

        loop {
            let mut macro_ctx = mem::take(&mut self.state.macro_ctx);
            self.load_macros(&mut macro_ctx, macros.iter().copied(), task, jit_ctx);
            let Some(grouped_items) = self.parse::<GroupedItemsAst>(source, arena, task, Some(&macro_ctx.tokens), shared) else {
                continue;
            };

            self.type_check_batch(module, grouped_items, arena, task, shared);
            let last = grouped_items.last;
            arena.clear();

            self.state.macro_ctx = macro_ctx.clear();
            let mut local_macros = mem::take(&mut self.state.tir_builder_ctx.macros);
            self.compile_macros(&local_macros, task, jit_ctx, shared);
            macros.extend(local_macros.drain(..));
            self.state.tir_builder_ctx.macros = local_macros;

            if last {
                break;
            }
        }
    }

    fn compile_macros(
        &mut self,
        macros: &[MacroCompileRequest],
        task: &mut Task,
        jit_ctx: &mut JitContext,
        shared: &Shared,
    ) {
        self.push_macro_compile_requests(macros, task, shared);
        let mut layouts = mem::take(&mut self.state.jit_layouts);
        let compiled = self.compile_current_requests(task, shared, shared.jit_isa, &mut layouts);
        self.state.jit_layouts = layouts;

        jit_ctx
            .load_functions(compiled, &task.gen, &task.typec, &task.interner, false)
            .expect("Hmm lets reconsider our life choices...");
    }

    fn compile_current_requests(
        &mut self,
        task: &mut Task,
        shared: &Shared,
        isa: &Isa,
        gen_layouts: &mut GenLayouts,
    ) -> BumpVec<VRef<CompiledFunc>> {
        let mut compiled = bumpvec![];
        while let Some(request) = self.state.compile_requests.queue.pop() {
            let CompileRequest { func, id, params } = request;
            let Func { signature, .. } = task.typec.funcs[func];
            let body = &task.mir.bodies[func].inner;
            let params = self.state.compile_requests.ty_slices[params].to_bumpvec();
            self.state.temp_dependant_types.clear();
            self.state
                .temp_dependant_types
                .extend(task.mir.bodies[func].dependant_types.values().copied());
            Self::swap_mir_types(
                body,
                &mut self.state.temp_dependant_types,
                &params,
                &mut task.typec,
                &mut task.interner,
            );
            let mut builder = GenBuilder::new(
                isa,
                body,
                &mut self.context.func,
                &self.state.temp_dependant_types,
                &mut self.function_builder_ctx,
            );
            let root = body.blocks.keys().next().expect("Better try next time!");
            Generator::new(
                &mut self.state.compile_requests,
                gen_layouts,
                &mut task.gen,
                &mut self.state.gen_resources,
                &mut task.interner,
                &mut task.typec,
                shared.resources,
            )
            .generate(signature, &params, root, &mut builder);

            task.gen
                .save_compiled_code(id, &self.context)
                .expect("Superb error occurred!");
            compiled.push(id);
        }
        self.state.compile_requests.clear();
        compiled
    }

    fn push_macro_compile_requests(
        &mut self,
        macros: &[MacroCompileRequest],
        task: &mut Task,
        shared: &Shared,
    ) {
        for &MacroCompileRequest { ty, r#impl, .. } in macros {
            let Impl {
                generics,
                methods,
                key: ImplKey { ty: template, .. },
                ..
            } = task.typec.impls[r#impl];

            let mut params = bumpvec![None; task.typec.params[generics].len()];
            task.typec
                .compatible(&mut params, ty, template)
                .expect("Heh...");
            let params = params
                .into_iter()
                .collect::<Option<BumpVec<_>>>()
                .expect("Lovely!");

            for &func in &task.typec.func_slices[methods] {
                let key = Generator::func_instance_name(
                    true,
                    shared.jit_isa.triple,
                    func,
                    params.iter().cloned(),
                    &task.typec,
                    &mut task.interner,
                );
                let id = task
                    .gen
                    .compiled_funcs
                    .insert_unique(key, CompiledFunc::new(func));
                self.state.compile_requests.queue.push(CompileRequest {
                    id,
                    func,
                    params: self.state.compile_requests.ty_slices.bump_slice(&params),
                });
            }
        }
    }

    fn load_macros<'macros>(
        &mut self,
        ctx: &mut MacroCtx<'macros>,
        token_macros: impl IntoIterator<Item = MacroCompileRequest>,
        task: &mut Task,
        jit_ctx: &'macros JitContext,
    ) {
        for MacroCompileRequest { name, r#impl, .. } in token_macros {
            let impl_ent = task.typec.impls[r#impl];
            let spec = impl_ent.key.spec.base(&task.typec);

            match spec {
                s if s == SpecBase::TOKEN_MACRO => {
                    if let Some(spec) = self.state.token_macros.get(r#impl) {
                        let tm = jit_ctx.token_macro(spec).expect("Well then...");
                        ctx.tokens.declare_macro(spec.name, tm);
                        continue;
                    }
                }
                _ => unreachable!(),
            }

            let layout = self.state.jit_layouts.ty_layout(
                impl_ent.key.ty,
                &[],
                &mut task.typec,
                &mut task.interner,
            );
            let funcs = task.typec.func_slices[impl_ent.methods]
                .iter()
                .map(|&func| task.typec.funcs[func].name)
                .filter_map(|name| task.gen.compiled_funcs.index(name));

            match spec {
                s if s == SpecBase::TOKEN_MACRO => {
                    let r#macro = TokenMacroOwnedSpec::new(layout.rust_layout(), name, funcs)
                        .expect("Deng it!");
                    let tm = jit_ctx.token_macro(&r#macro).expect("That sucks...");
                    ctx.tokens.declare_macro(r#macro.name, tm);
                    self.state.token_macros.insert(r#impl, r#macro);
                }
                _ => unreachable!(),
            }
        }
    }

    fn type_check_batch(
        &mut self,
        module: VRef<Module>,
        grouped_items: GroupedItemsAst,
        arena: &Arena,
        task: &mut Task,
        shared: &Shared,
    ) {
        let mut type_checked_funcs = bumpvec![];
        TyChecker::new(
            module,
            &mut task.interner,
            &mut self.state.scope,
            &mut task.typec,
            &mut task.workspace,
            shared.resources,
        )
        .execute(
            arena,
            grouped_items,
            &mut self.state.ty_checker_ctx,
            &mut self.state.tir_builder_ctx,
            self.state.ast_transfer.activate(),
            &mut type_checked_funcs,
        );

        MirChecker::new(
            module,
            &mut task.mir,
            &mut task.interner,
            &mut task.typec,
            &mut task.workspace,
            shared.resources,
        )
        .funcs(&mut self.state.mir_builder_ctx, &mut type_checked_funcs);
        task.entry_points.extend(
            self.state
                .mir_builder_ctx
                .just_compiled
                .drain(..)
                .filter(|&func| task.typec.funcs[func].flags.contains(FuncFlags::ENTRY)),
        );
    }

    fn parse<'a, T: Ast<'a>>(
        &mut self,
        source: VRef<Source>,
        arena: &'a Arena,
        task: &mut Task,
        macros: Option<&TokenMacroCtx>,
        shared: &Shared,
    ) -> Option<T>
    where
        T::Args: Default,
    {
        ParsingCtx::new_with_macros(
            shared.resources.sources[source].content.as_str(),
            &mut self.state.parsing_state,
            arena,
            &mut task.workspace,
            &mut task.interner,
            macros,
            source,
        )
        .parse()
    }

    pub fn swap_mir_types(
        func: &FuncMirInner,
        dependant_types: &mut [MirTy],
        params: &[Ty],
        typec: &mut Typec,
        interner: &mut Interner,
    ) {
        if params.is_empty() {
            return;
        }

        for &mir_ty in &func.ty_params[func.generics] {
            let ty = dependant_types[mir_ty.index()].ty;
            let new_ty = typec.instantiate(ty, params, interner);
            dependant_types[mir_ty.index()].ty = new_ty;
        }
    }
}

#[derive(Default)]
pub struct GenTask {}

#[derive(Default)]
pub struct Task {
    pub modules_to_compile: Vec<VRef<Module>>,
    pub entry_points: Vec<VRef<Func>>,
    pub interner: Interner,
    pub workspace: Workspace,
    pub typec: Typec,
    pub mir: Mir,
    pub gen: Gen,
}

impl Task {
    fn clear(&mut self) {
        self.modules_to_compile.clear();
        self.interner.clear();
        self.typec.clear();
        self.mir.clear();
        self.gen.clear();
    }
}

#[derive(Default)]
pub struct WorkerState {
    pub parsing_state: ParsingState,
    pub scope: Scope,
    pub ty_checker_ctx: TyCheckerCtx,
    pub ast_transfer: AstTransfer<'static>,
    pub mir_builder_ctx: MirBuilderCtx,
    pub token_macros: SparseMap<Impl, TokenMacroOwnedSpec>,
    pub macro_ctx: MacroCtx<'static>,
    pub tir_builder_ctx: TirBuilderCtx,
    pub compile_requests: CompileRequests,
    pub temp_dependant_types: Vec<MirTy>,
    pub gen_resources: GenResources,
    pub jit_layouts: GenLayouts,
}

#[derive(Default)]
pub struct MacroCtx<'macros> {
    pub tokens: TokenMacroCtx<'macros>,
}

impl<'macros> MacroCtx<'macros> {
    pub fn clear<'detached>(self) -> MacroCtx<'detached> {
        MacroCtx {
            tokens: self.tokens.clear(),
        }
    }
}
