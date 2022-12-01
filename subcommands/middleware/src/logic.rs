use std::{
    collections::{hash_map::Entry, VecDeque},
    default::default,
    fmt::Write,
    iter, mem,
    path::*,
    slice,
    sync::mpsc::{self, Receiver, Sender, SyncSender},
    thread,
};

use cranelift_codegen::{
    ir::{self, InstBuilder},
    isa::CallConv,
    Context,
};
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

#[derive(Default)]
pub struct Middleware {
    pub workspace: Workspace,
    pub resources: Resources,
    pub package_graph: PackageGraph,
    pub resource_loading_ctx: PackageLoaderCtx,
    pub task_graph: TaskGraph,
    pub entry_points: Vec<FragRef<Func>>,
}

impl Middleware {
    pub fn new() -> Self {
        Self::default()
    }

    fn reload_resources(&mut self, main_task: &mut Task, path: &Path) {
        PackageLoader::new(
            &mut self.resources,
            &mut main_task.workspace,
            &mut main_task.interner,
            &mut self.package_graph,
        )
        .reload(path, &mut self.resource_loading_ctx);
    }

    pub fn update(&mut self, args: &MiddlewareArgs) -> Option<MiddlewareOutput> {
        let mut ir = args.dump_ir.then(String::new);
        let mut main_task = Task {
            ir_dump: ir.clone(),
            ..default()
        };

        self.reload_resources(&mut main_task, &args.path);
        main_task.typec.init(&mut main_task.interner);

        if main_task.workspace.has_errors() {
            return None;
        }

        let (sender, mut receiver) = mpsc::channel();
        let num_workers = thread::available_parallelism()
            .map_or(1, |n| n.get())
            .min(args.max_cores.unwrap_or(usize::MAX));

        if num_workers == 1 {
            todo!();
        }

        let workers: Vec<_> = iter::repeat_with(|| Worker::new(sender.clone()))
            .take(num_workers)
            .collect();

        self.build_task_graph();

        let resources = mem::take(&mut self.resources);

        let tasks = vec![main_task; num_workers];

        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
        };

        let binary = thread::scope(|s| {
            let mut senders = workers
                .into_iter()
                .enumerate()
                // the main_task is actually last in tasks but we want it first
                .zip(tasks.into_iter().rev())
                .map(|((id, (worker, sender)), task)| {
                    worker.run(s, shared);
                    (sender, Some(Task { id, ..task }))
                })
                .collect::<Vec<_>>();

            let mut tasks = self.expand(&mut receiver, &mut senders, &resources);
            let (entry_points, imported) = self.distribute_compile_requests(&mut tasks, &args.isa);
            for (mut task, (recv, ..)) in tasks.into_iter().zip(senders.iter_mut()) {
                task.for_generation = true;
                recv.send(task).expect("Bruh...");
            }

            let mut tasks = (0..num_workers)
                .map(|_| receiver.recv().expect("Eh..."))
                .collect::<BumpVec<_>>();
            tasks.sort_unstable_by_key(|t| t.id);
            let mut main_task = tasks
                .into_iter()
                .reduce(|mut acc, mut task| {
                    if let (Some(ir), Some(dump)) = (&mut acc.ir_dump, task.ir_dump) {
                        ir.push_str(&dump);
                    }
                    acc.workspace.transfer(&mut task.workspace);
                    acc.compile_requests
                        .queue
                        .extend(task.compile_requests.queue);
                    acc
                })
                .expect("I thought we checked this...");

            let entry_point = self.generate_entry_point(&mut main_task, &args.isa, entry_points);

            let mut object = ObjectContext::new(&args.isa).expect("This really sucks...");
            object
                .load_functions(
                    main_task
                        .compile_requests
                        .queue
                        .into_iter()
                        .map(|req| req.id)
                        .chain(imported)
                        .chain(iter::once(entry_point)),
                    &main_task.gen,
                    &main_task.typec,
                    &main_task.interner,
                )
                .map_err(|e| {
                    if let ObjectRelocationError::MissingSymbol(f) = e {
                        let func = main_task.gen.funcs[f].func;
                        let name = &main_task.interner[main_task.typec.funcs[func].name];
                        eprintln!("Missing symbol: {}", name);
                    }
                    e
                })
                .expect("So close...");

            self.workspace = main_task.workspace;
            ir = main_task.ir_dump;
            object.emit().expect("This is so sad...")
        });

        self.resources = resources;

        Some(MiddlewareOutput { binary, ir })
    }

    fn generate_entry_point(
        &mut self,
        main_task: &mut Task,
        isa: &Isa,
        entry_points: BumpVec<FragRef<CompiledFunc>>,
    ) -> FragRef<CompiledFunc> {
        let mut context = Context::new();
        let mut func_ctx = FunctionBuilderContext::new();

        let (body, dependant) = (default(), default());
        let mut builder = GenBuilder::new(isa, &body, &mut context.func, &dependant, &mut func_ctx);

        let entry_point = builder.create_block();
        builder.append_block_params_for_function_params(entry_point);
        builder.switch_to_block(entry_point);

        for index in entry_points {
            let signature = ir::Signature::new(CallConv::Fast);
            let sig_ref = builder.import_signature(signature);
            let external_name = ir::UserExternalName::new(0, index.to_u32());
            let name = builder.func.declare_imported_user_function(external_name);
            let func_ref = builder.import_function(ir::ExtFuncData {
                name: ir::ExternalName::user(name),
                signature: sig_ref,
                colocated: true,
            });
            builder.ins().call(func_ref, &[]);
        }
        builder.ins().return_(&[]);

        let func_id = main_task.typec.funcs.push(Func {
            visibility: FuncVisibility::Exported,
            name: main_task.interner.intern(gen::ENTRY_POINT_NAME),
            ..default()
        });
        let entry_point = main_task.gen.funcs.push(CompiledFunc::new(
            func_id,
            main_task.interner.intern(gen::ENTRY_POINT_NAME),
        ));

        context.compile(&*isa.inner).expect("Real nice...");
        main_task
            .gen
            .save_compiled_code(entry_point, &context)
            .expect("Not again...");

        entry_point
    }

    pub fn distribute_compile_requests(
        &mut self,
        tasks: &mut [Task],
        isa: &Isa,
    ) -> (
        BumpVec<FragRef<CompiledFunc>>, // in executable
        BumpVec<FragRef<CompiledFunc>>, // in lib or dll
    ) {
        let task_distribution = (0..tasks.len()).cycle();
        let distributor = |(&func, task_id): (_, usize)| {
            let main_task = &mut tasks[0];
            let key = Generator::func_instance_name(
                isa.jit,
                &isa.triple,
                func,
                iter::empty(),
                &main_task.typec,
                &mut main_task.interner,
            );
            let id = tasks[task_id].gen.funcs.push(CompiledFunc::new(func, key));
            (
                CompileRequestChild {
                    id,
                    func,
                    params: default(),
                },
                Some(task_id),
            )
        };
        let frontier = self
            .entry_points
            .iter()
            .zip(task_distribution)
            .map(distributor)
            .collect::<BumpVec<_>>();

        let internal = frontier.iter().map(|(req, _)| req.id).collect();
        let external = Task::traverse_compile_requests(frontier, tasks, isa);
        (internal, external)
    }

    pub fn expand(
        &mut self,
        receiver: &mut Receiver<Task>,
        senders: &mut [(SyncSender<Task>, Option<Task>)],
        resources: &Resources,
    ) -> Vec<Task> {
        let mut module_items = ShadowMap::new();

        fn sync_module_items(
            target: &mut ShadowMap<Module, ModuleItems>,
            source: &ShadowMap<Module, ModuleItems>,
        ) {
            for (k, v) in source.iter() {
                let mv = &mut target[k];
                if mv.items.is_empty() {
                    mv.items = v.items.clone();
                }
            }
        }

        let mut packages = bumpvec![None; senders.len()];

        while self.task_graph.has_tasks() {
            for (sender, maybe_task) in senders.iter_mut() {
                let Some(mut task) = maybe_task.take() else {
                    continue;
                };

                let Some(package) = self.task_graph.next_task() else {
                    *maybe_task = Some(task);
                    break;
                };

                resources
                    .module_order
                    .iter()
                    .filter(|&&module| resources.modules[module].package == package)
                    .collect_into(&mut task.modules_to_compile);

                sync_module_items(&mut task.typec.module_items, &module_items);

                packages[task.id] = Some(package);
                sender.send(task).expect("Whoops...");
            }

            let mut task = receiver.recv().expect("Very bad...");
            loop {
                self.entry_points.append(&mut task.entry_points);
                sync_module_items(&mut module_items, &task.typec.module_items);
                task.modules_to_compile.clear();
                let id = task.id;
                senders[id].1 = Some(task);
                self.task_graph.finish(packages[id].take().expect("Why??"));
                if let Ok(next_task) = receiver.try_recv() {
                    task = next_task;
                } else {
                    break;
                }
            }
        }

        senders
            .iter_mut()
            .map(|(.., task)| {
                task.take()
                    .unwrap_or_else(|| receiver.recv().expect("We are doomed!"))
            })
            .collect::<Vec<_>>()
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

        self.resources
            .packages
            .values()
            .map(|package| self.resources.package_deps[package.deps].len())
            .map(|count| (count, default()))
            .collect_into(&mut self.task_graph.meta);

        for package_deps in edges.group_by(|(a, _), (b, _)| *a == *b) {
            let package = package_deps[0].0;

            let (count, ref mut children) = self.task_graph.meta[package.index()];
            *children = self
                .task_graph
                .inverse_graph
                .extend(package_deps.iter().map(|&(_, dep)| dep));
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
    inverse_graph: PushMap<VRef<Package>>,
    done: usize,
}

impl TaskGraph {
    pub fn next_task(&mut self) -> Option<VRef<Package>> {
        self.frontier.pop_front()
    }

    pub fn finish(&mut self, package: VRef<Package>) {
        self.done += 1;
        let (.., deps) = self.meta[package.index()];
        for &dep in &self.inverse_graph[deps] {
            let index = dep.index();
            self.meta[index].0 -= 1;
            if self.meta[index].0 == 0 {
                self.frontier.push_back(dep);
            }
        }
    }

    pub fn clear(&mut self) {
        self.meta.clear();
        self.frontier.clear();
        self.inverse_graph.clear();
    }

    fn has_tasks(&self) -> bool {
        self.meta.len() != self.done
    }
}

#[derive(Clone, Copy)]
pub struct Shared<'a> {
    resources: &'a Resources,
    jit_isa: &'a Isa,
    isa: &'a Isa,
}

pub struct MiddlewareArgs {
    pub path: PathBuf,
    pub jit_isa: Isa,
    pub isa: Isa,
    pub incremental_path: Option<PathBuf>,
    pub max_cores: Option<usize>,
    pub dump_ir: bool,
}

pub struct MiddlewareOutput {
    pub binary: Vec<u8>,
    pub ir: Option<String>,
}

pub struct Worker {
    pub tasks: Receiver<Task>,
    pub products: Sender<Task>,
    pub state: WorkerState,
    pub context: Context,
    pub function_builder_ctx: FunctionBuilderContext,
}

impl Worker {
    pub fn new(products: Sender<Task>) -> (Self, SyncSender<Task>) {
        let (tx, rx) = mpsc::sync_channel(0);
        (
            Self {
                tasks: rx,
                products,
                state: WorkerState::default(),
                context: Context::new(),
                function_builder_ctx: FunctionBuilderContext::new(),
            },
            tx,
        )
    }

    pub fn run<'a: 'b, 'b>(mut self, thread_scope: &'a thread::Scope<'b, '_>, shared: Shared<'b>) {
        thread_scope.spawn(move || {
            let mut arena = Arena::new();
            let mut jit_ctx = JitContext::new(iter::empty());
            self.state.jit_layouts.ptr_ty = shared.jit_isa.pointer_ty;
            self.state.gen_layouts.ptr_ty = shared.isa.pointer_ty;
            let mut compile_task = loop {
                let mut task = self.tasks.recv().expect("This is f...");
                if task.for_generation {
                    break task;
                }

                let modules = mem::take(&mut task.modules_to_compile);
                for &module in modules.iter() {
                    self.compile_module(module, &mut arena, &mut task, &mut jit_ctx, &shared);
                }
                task.modules_to_compile = modules;
                self.products.send(task).expect("As I was saying...");
            };

            let mut gen_layouts = mem::take(&mut self.state.gen_layouts);
            self.compile_current_requests(&mut compile_task, &shared, shared.isa, &mut gen_layouts);
            self.products
                .send(compile_task)
                .expect("Seems like it all was useless.");
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

        self.state
            .parsing_state
            .start(&shared.resources.sources[source].content);
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
            self.load_macros(
                &mut macro_ctx,
                macros.iter().copied(),
                task,
                jit_ctx,
                shared.jit_isa,
            );
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
        if macros.is_empty() {
            return;
        }

        let imported = self.push_macro_compile_requests(macros, task, shared);
        let mut layouts = mem::take(&mut self.state.jit_layouts);
        let compiled = self.compile_current_requests(task, shared, shared.jit_isa, &mut layouts);
        self.state.jit_layouts = layouts;

        jit_ctx
            .load_functions(
                compiled.into_iter().chain(imported),
                &task.gen,
                &task.typec,
                &task.interner,
                false,
            )
            // .map_err(|e| {
            //     if let JitRelocError::MissingSymbol(GenItemName::Func(func)) = e {
            //         dbg!(&task.interner[task.typec[task.gen.funcs[func].func].name]);
            //     }
            //     e
            // })
            .expect("Hmm lets reconsider our life choices...");
        jit_ctx.prepare_for_execution();
    }

    fn compile_current_requests(
        &mut self,
        task: &mut Task,
        shared: &Shared,
        isa: &Isa,
        gen_layouts: &mut GenLayouts,
    ) -> BumpVec<FragRef<CompiledFunc>> {
        let mut compiled = bumpvec![];
        for &CompileRequest {
            func,
            id,
            params,
            children,
        } in &task.compile_requests.queue
        {
            compiled.push(id);
            let Func { signature, .. } = task.typec.funcs[func];

            let body = task
                .mir
                .bodies
                .get(&func)
                .expect("here we go again")
                .to_owned();
            let params = task.compile_requests.ty_slices[params].to_bumpvec();
            self.state.temp_dependant_types.clear();
            self.state
                .temp_dependant_types
                .extend(body.types.values().copied().skip(MirTy::ALL.len()));
            Self::swap_mir_types(
                &body,
                &mut self.state.temp_dependant_types,
                &params,
                &mut task.typec,
                &mut task.interner,
            );
            let mut builder = GenBuilder::new(
                isa,
                &body,
                &mut self.context.func,
                &self.state.temp_dependant_types,
                &mut self.function_builder_ctx,
            );
            let root = body.blocks.keys().next().expect("Better try next time!");
            self.state.gen_resources.calls.clear();
            self.state
                .gen_resources
                .calls
                .extend(task.compile_requests.children[children].iter().copied());
            // dbg!(&task.interner[task.typec.funcs[func].name]);
            Generator::new(
                gen_layouts,
                &mut task.gen,
                &mut self.state.gen_resources,
                &mut task.interner,
                &mut task.typec,
                &task.compile_requests,
                shared.resources,
            )
            .generate(signature, &params, root, &mut builder);
            if let Some(ref mut dump) = task.ir_dump {
                let name = &task.interner[task.typec.funcs[func].name];
                write!(dump, "{} {}", name, self.context.func.display()).unwrap();
                // print!("{}", self.context.func.display());
            }
            self.context.compile(&*isa.inner).expect("Failure!");
            task.gen
                .save_compiled_code(id, &self.context)
                .expect("Superb error occurred!");
            self.context.clear();
        }
        if !task.for_generation {
            task.compile_requests.clear();
        }
        compiled
    }

    fn push_macro_compile_requests(
        &mut self,
        macros: &[MacroCompileRequest],
        task: &mut Task,
        shared: &Shared,
    ) -> BumpVec<FragRef<CompiledFunc>> {
        let extractor = |&MacroCompileRequest { ty, r#impl, .. }| {
            let Impl {
                generics,
                methods,
                key: ImplKey { ty: template, .. },
                ..
            } = task.typec.impls[r#impl];

            // infer params
            let mut params = bumpvec![None; generics.len()];
            task.typec
                .compatible(&mut params, ty, template)
                .expect("Heh...");
            let params = params
                .into_iter()
                .collect::<Option<BumpVec<_>>>()
                .expect("Lovely!");
            let pushed_params = task.compile_requests.ty_slices.bump_slice(&params);

            let collector = |&func| {
                let key = Generator::func_instance_name(
                    true,
                    &shared.jit_isa.triple,
                    func,
                    params.iter().cloned(),
                    &task.typec,
                    &mut task.interner,
                );
                let id = task.gen.funcs.push(CompiledFunc::new(func, key));
                task.gen.lookup.insert(key, id);
                (
                    CompileRequestChild {
                        func,
                        id,
                        params: pushed_params,
                    },
                    // we only have one thread
                    Some(0),
                )
            };
            let frontier = task.typec.func_slices[methods]
                .iter()
                .map(collector)
                .collect::<BumpVec<_>>();

            Task::traverse_compile_requests(frontier, slice::from_mut(task), shared.jit_isa)
        };

        macros.iter().flat_map(extractor).collect()
    }

    fn load_macros<'macros>(
        &mut self,
        ctx: &mut MacroCtx<'macros>,
        macros: impl IntoIterator<Item = MacroCompileRequest>,
        task: &mut Task,
        jit_ctx: &'macros JitContext,
        isa: &Isa,
    ) {
        if task.workspace.has_errors() {
            return;
        }

        for MacroCompileRequest {
            name, r#impl, ty, ..
        } in macros
        {
            let impl_ent = task.typec.impls[r#impl];
            let spec = impl_ent.key.spec.base(&task.typec);

            match spec {
                s if s == SpecBase::TOKEN_MACRO => {
                    if let Some(spec) = self.state.token_macros.get(&r#impl) {
                        let tm = jit_ctx.token_macro(spec).expect("Well then...");
                        ctx.tokens.declare_macro(spec.name, tm);
                        continue;
                    }
                }
                _ => unreachable!(),
            }

            let layout =
                self.state
                    .jit_layouts
                    .ty_layout(ty, &[], &mut task.typec, &mut task.interner);
            let mut infer_slots = bumpvec![None; impl_ent.generics.len()];
            task.typec
                .compatible(&mut infer_slots, ty, impl_ent.key.ty)
                .expect("This should work, right?");
            let params = infer_slots
                .into_iter()
                .collect::<Option<BumpVec<_>>>()
                .expect("Guess I am a hypocrite...");
            let funcs = task.typec.func_slices[impl_ent.methods]
                .iter()
                .map(|&func| {
                    Generator::func_instance_name(
                        true,
                        &isa.triple,
                        func,
                        params.iter().copied(),
                        &task.typec,
                        &mut task.interner,
                    )
                })
                .filter_map(|key| task.gen.lookup.get(&key).map(|entry| entry.to_owned()));

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
            &mut self.state.mir_ctx,
            &mut self.state.mir_move_ctx,
            arena,
            shared.resources,
        )
        .funcs(&mut type_checked_funcs)
        // .dbg_funcs()
        ;

        self.state
            .mir_ctx
            .just_compiled
            .drain(..)
            .filter(|&func| task.typec.funcs[func].flags.contains(FuncFlags::ENTRY))
            .collect_into(&mut task.entry_points);

        let source = shared.resources.modules[module].source;
        Self::check_casts(
            &mut task.typec,
            &mut task.interner,
            &mut self.state.gen_layouts,
            source,
            &mut task.workspace,
            &mut self.state.tir_builder_ctx.cast_checks,
        )
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
        dependant_types: &mut FuncTypes,
        params: &[Ty],
        typec: &mut Typec,
        interner: &mut Interner,
    ) {
        if params.is_empty() {
            return;
        }

        for &mir_ty in &func.ty_params[func.generics] {
            let ty = dependant_types[mir_ty].ty;
            let new_ty = typec.instantiate(ty, params, interner);
            dependant_types[mir_ty].ty = new_ty;
        }
    }

    pub fn check_casts(
        typec: &mut Typec,
        interner: &mut Interner,
        layouts: &mut GenLayouts,
        source: VRef<Source>,
        workspace: &mut Workspace,
        checks: &mut Vec<CastCheck>,
    ) {
        for CastCheck { loc, from, to } in checks.drain(..) {
            if typec.contains_params(from) || typec.contains_params(to) {
                workspace.push(snippet! {
                    err: "cast between generic types is not allowed";
                    info: (
                        "cast from {} to {}, which contain generic parameters that depend on function instance",
                        typec.display_ty(from, interner),
                        typec.display_ty(to, interner),
                    );
                    (loc, source) {
                        err[loc]: "happened here";
                    }
                });
                continue;
            }

            let from_layout = layouts.ty_layout(from, &[], typec, interner);
            let to_layout = layouts.ty_layout(to, &[], typec, interner);
            if from_layout.size != to_layout.size {
                workspace.push(snippet! {
                    err: "cast size mismatch";
                    info: (
                        "cast from {}({}) to {}({}), size does not match",
                        typec.display_ty(from, interner),
                        from_layout.size,
                        typec.display_ty(to, interner),
                        to_layout.size,
                    );
                    (loc, source) {
                        err[loc]: "happened here";
                    }
                });
            }
        }
    }
}

#[derive(Default)]
pub struct GenTask {}

#[derive(Default)]
pub struct Task {
    // config
    pub id: usize,
    pub for_generation: bool,
    pub ir_dump: Option<String>,

    // temp
    pub compile_requests: CompileRequests,
    pub modules_to_compile: Vec<VRef<Module>>,
    pub entry_points: Vec<FragRef<Func>>,
    pub workspace: Workspace,

    // state
    pub interner: Interner,
    pub typec: Typec,
    pub mir: Mir,
    pub gen: Gen,
}

impl Task {
    fn traverse_compile_requests(
        mut frontier: BumpVec<(CompileRequestChild, Option<usize>)>,
        tasks: &mut [Task],
        isa: &Isa,
    ) -> BumpVec<FragRef<CompiledFunc>> {
        let mut seen = Map::default();
        let mut cycle = (0..tasks.len()).cycle();
        let mut imported = bumpvec![];
        while let Some((CompileRequestChild { id, func, params }, task_id)) = frontier.pop() {
            let Some(task_id) = task_id else { continue; };
            let task = &mut tasks[task_id];
            let Func {
                flags, visibility, ..
            } = task.typec[func];
            if flags.contains(FuncFlags::BUILTIN) {
                continue;
            }
            if visibility == FuncVisibility::Imported {
                imported.push(id);
                continue;
            }

            let body = task
                .mir
                .bodies
                .get(&func)
                .expect("Effing amazing..")
                .clone();
            let mut types = body.types.clone();
            let bumped_params = task.compile_requests.ty_slices[params].to_bumpvec();
            Worker::swap_mir_types(
                &body.inner,
                &mut types,
                &bumped_params,
                &mut task.typec,
                &mut task.interner,
            );
            let prev = frontier.len();
            body.calls
                .values()
                .zip(cycle.by_ref())
                .map(|(&call, task_id)| {
                    tasks[task_id].load_call(&body, &types, call, task_id, isa, &mut seen)
                })
                .collect_into(&mut *frontier);

            let task = &mut tasks[task_id];
            let children = task
                .compile_requests
                .children
                .extend(frontier[prev..].iter().map(|&(child, _)| child));
            task.compile_requests.queue.push(CompileRequest {
                func,
                id,
                params,
                children,
            });
        }

        imported
    }

    fn load_call(
        &mut self,
        body: &FuncMir,
        types: &FuncTypes,
        CallMir {
            callable, params, ..
        }: CallMir,
        task_id: usize,
        isa: &Isa,
        seen: &mut Map<FragSlice<u8>, FragRef<CompiledFunc>>,
    ) -> (CompileRequestChild, Option<usize>) {
        let params = body.ty_params[params]
            .iter()
            .map(|&ty| types[ty].ty)
            .collect::<BumpVec<_>>();
        let (func_id, params) = match callable {
            CallableMir::Func(func_id) => (func_id, params),
            CallableMir::SpecFunc(func) => self.load_spec_func(func, &params),
            CallableMir::Pointer(_) => todo!(),
        };

        let key = Generator::func_instance_name(
            false,
            &isa.triple,
            func_id,
            params.iter().cloned(),
            &self.typec,
            &mut self.interner,
        );
        let entry = seen.entry(key);
        let task_id = matches!(entry, Entry::Occupied(..)).then_some(task_id);
        let &mut id = entry.or_insert_with(|| self.gen.funcs.push(CompiledFunc::new(func_id, key)));

        (
            CompileRequestChild {
                func: func_id,
                id,
                params: self.compile_requests.ty_slices.bump_slice(&params),
            },
            task_id,
        )
    }

    fn load_spec_func(
        &mut self,
        func: FragRef<SpecFunc>,
        params: &[Ty],
    ) -> (FragRef<Func>, BumpVec<Ty>) {
        // TODO: I dislike how much can panic here, maybe improve this in the future
        let SpecFunc {
            parent, generics, ..
        } = self.typec.spec_funcs[func];
        let SpecBase { methods, .. } = self.typec[parent];
        let index = methods.keys().position(|key| key == func).unwrap();
        let generic_count = params.len() - generics.len();
        let (upper, caller, lower) = (
            &params[..generic_count - 1],
            params[generic_count - 1],
            &params[generic_count..],
        );
        let used_spec = if upper.is_empty() {
            Spec::Base(parent)
        } else {
            Spec::Instance(self.typec.spec_instance(parent, upper, &mut self.interner))
        };

        let r#impl = self
            .typec
            .find_implementation(caller, used_spec, &[], &mut None, &mut self.interner)
            .expect("ba")
            .expect("ka");
        let Impl {
            generics,
            methods,
            key: ImplKey { ty, spec },
            ..
        } = self.typec.impls[r#impl];
        let func_id = self.typec.func_slices[methods][index];
        let mut infer_slots = bumpvec![None; generics.len()];
        self.typec
            .compatible(&mut infer_slots, caller, ty)
            .expect("I have been lied to!");
        self.typec
            .compatible_spec(&mut infer_slots, used_spec, spec)
            .expect("Did you get it?");
        let params = infer_slots
            .iter()
            .map(|&slot| slot.expect("Why are you doing this?"))
            .chain(lower.iter().copied())
            .collect::<BumpVec<_>>();
        (func_id, params)
    }
}

impl Clone for Task {
    fn clone(&self) -> Self {
        Self {
            ir_dump: self.ir_dump.clone(),

            interner: self.interner.clone(),
            typec: self.typec.clone(),
            mir: self.mir.clone(),
            gen: self.gen.clone(),

            ..default()
        }
    }
}

#[derive(Default)]
pub struct WorkerState {
    pub parsing_state: ParsingState,
    pub scope: Scope,
    pub ty_checker_ctx: TyCheckerCtx,
    pub ast_transfer: AstTransfer<'static>,
    pub mir_ctx: MirCtx,
    pub mir_move_ctx: MirMoveCtx,
    pub token_macros: Map<FragRef<Impl>, TokenMacroOwnedSpec>,
    pub macro_ctx: MacroCtx<'static>,
    pub tir_builder_ctx: TirBuilderCtx,
    pub temp_dependant_types: FuncTypes,
    pub gen_resources: GenResources,
    pub jit_layouts: GenLayouts,
    pub gen_layouts: GenLayouts,
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
