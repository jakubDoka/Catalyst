use std::{
    collections::{hash_map::Entry, VecDeque},
    default::default,
    fmt::Write,
    iter, mem,
    path::*,
    slice,
    sync::mpsc::{self, Receiver, Sender, SyncSender},
    thread::{self, ScopedJoinHandle},
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
    pub package_graph: PackageGraph,
    pub resource_loading_ctx: ResourceLoaderCtx,
    pub task_graph: TaskGraph,
    pub entry_points: Vec<FragRef<Func>>,
    pub incremental: Option<Incremental>,
    pub relocator: TypecRelocator,
}

impl Middleware {
    pub fn new() -> Self {
        Self::default()
    }

    fn reload_resources(
        &mut self,
        resources: &mut Resources,
        main_task: &mut Task,
        path: &Path,
    ) -> Option<Vec<VRef<Source>>> {
        PackageLoader::new(
            resources,
            &mut main_task.workspace,
            &mut main_task.interner,
            &mut self.package_graph,
        )
        .reload(path, &mut self.resource_loading_ctx)
    }

    pub fn update(&mut self, args: &MiddlewareArgs) -> Option<MiddlewareOutput> {
        self.workspace.clear();

        let Incremental {
            mut resources,
            mut main_task,
            mut worker_pool,
            mut module_items,
        } = self.incremental.take().unwrap_or_default();

        main_task.ir_dump = args.dump_ir.then(String::new);

        if let Some(removed) = self.reload_resources(&mut resources, &mut main_task, &args.path) {
            self.sweep_resources(&resources, &mut main_task, &mut module_items, removed);
        };

        main_task.typec.init(&mut main_task.interner);

        if main_task.workspace.has_errors() {
            self.workspace.transfer(&mut main_task.workspace);
            self.incremental = Some(Incremental {
                resources,
                main_task,
                worker_pool,
                module_items,
            });
            return None;
        }

        let (package_sender, package_receiver) = mpsc::channel();
        let workers_count = thread::available_parallelism()
            .map_or(1, |n| n.get())
            .min(args.max_cores.unwrap_or(usize::MAX));

        if workers_count == 1 {
            // todo!();
        }

        let workers = iter::repeat_with(|| WorkerConnections::new(package_sender.clone()))
            .take(workers_count)
            .collect::<Vec<_>>();

        self.build_task_graph(&resources);

        // the vec![] internals will put the main_task last, this is important
        // since later when resolving first dependency we want the main task first
        // (we pop from this later)
        let mut tasks = vec![main_task; workers_count];
        tasks
            .iter_mut()
            .enumerate()
            .for_each(|(i, task)| task.id = i);

        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
        };

        let (mut tasks, entry_points, imported) = thread::scope(|s| {
            let (mut package_tasks, mut gen_senders, mut threads) = (
                Vec::with_capacity(workers_count),
                Vec::with_capacity(workers_count),
                Vec::with_capacity(workers_count),
            );
            for ((connections, package_sender, gen_sender), task) in workers.into_iter().zip(tasks)
            {
                package_tasks.push(PackageTask {
                    self_sender: package_sender,
                    task,
                });
                gen_senders.push(gen_sender);
                threads.push(
                    worker_pool
                        .pop()
                        .unwrap_or_default()
                        .run(s, shared, connections),
                );
            }

            let mut tasks = self.expand(package_receiver, package_tasks, &resources, workers_count);

            for task in tasks.iter_mut() {
                self.workspace.transfer(&mut task.workspace);
            }

            if self.workspace.has_errors() {
                drop(gen_senders);
                worker_pool.extend(
                    threads
                        .into_iter()
                        .map(|thread| thread.join().expect("worker panicked").0),
                );
                return (tasks, vec![], vec![]);
            }

            let (entry_points, imported) = self.distribute_compile_requests(&mut tasks, &args.isa);
            for (task, recv) in tasks.into_iter().zip(gen_senders.iter_mut()) {
                recv.send(task).expect("worker terminated prematurely");
            }

            (
                threads
                    .into_iter()
                    .map(|thread| {
                        let (worker, task) = thread.join().expect("worker panicked");
                        worker_pool.push(worker);
                        task.expect("impossible since gen_senders still hasn't been dropped")
                    })
                    .collect::<Vec<_>>(),
                entry_points,
                imported,
            )
        });

        // we want consistent output during tests
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
            .expect("zero worker situation is impossible");

        let entry_point = self.generate_entry_point(&mut main_task, &args.isa, entry_points);

        let mut object = ObjectContext::new(&args.isa).unwrap();
        object
            .load_functions(
                main_task
                    .compile_requests
                    .queue
                    .drain(..)
                    .map(|req| req.id)
                    .chain(imported)
                    .chain(iter::once(entry_point)),
                &main_task.gen,
                &main_task.typec,
                &main_task.interner,
            )
            .unwrap();

        let ir = main_task.ir_dump.take();

        self.workspace.transfer(&mut main_task.workspace);
        self.incremental = Some(Incremental {
            resources,
            main_task,
            worker_pool,
            module_items,
        });

        Some(MiddlewareOutput {
            binary: object.emit().unwrap(),
            ir,
        })
    }

    fn generate_entry_point(
        &mut self,
        main_task: &mut Task,
        isa: &Isa,
        entry_points: Vec<FragRef<CompiledFunc>>,
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

        context.compile(&*isa.inner).unwrap();
        main_task
            .gen
            .save_compiled_code(entry_point, &context)
            .unwrap();

        entry_point
    }

    pub fn distribute_compile_requests(
        &mut self,
        tasks: &mut [Task],
        isa: &Isa,
    ) -> (
        Vec<FragRef<CompiledFunc>>, // in executable
        Vec<FragRef<CompiledFunc>>, // in lib or dll
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
        (internal, external.to_vec())
    }

    pub fn expand(
        &mut self,
        receiver: Receiver<(PackageTask, VRef<Package>)>,
        mut tasks: Vec<PackageTask>,
        resources: &Resources,
        worker_count: usize,
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

        while self.task_graph.has_tasks() {
            while let Some(mut package_task) = tasks.pop() {
                let Some(package) = self.task_graph.next_task() else {
                    tasks.push(package_task);
                    break;
                };

                package_task.task.modules_to_compile.clear();
                resources
                    .module_order
                    .iter()
                    .filter(|&&module| resources.modules[module].package == package)
                    .collect_into(&mut package_task.task.modules_to_compile);

                sync_module_items(&mut package_task.task.typec.module_items, &module_items);
                package_task
                    .send(package)
                    .expect("worker thread terminated prematurely");
            }

            let (mut package_task, mut package) = receiver
                .recv()
                .expect("All worker threads terminated prematurely");
            loop {
                self.entry_points
                    .append(&mut package_task.task.entry_points);
                sync_module_items(&mut module_items, &package_task.task.typec.module_items);
                self.task_graph.finish(package);
                tasks.push(package_task);
                if let Ok(next_task) = receiver.try_recv() {
                    (package_task, package) = next_task;
                } else {
                    break;
                }
            }
        }

        let leftover_tasks = receiver
            .into_iter()
            .take(worker_count - tasks.len())
            .map(|(task, ..)| task.task);
        tasks
            .into_iter()
            .map(|task| task.task)
            .chain(leftover_tasks)
            .collect()
    }

    pub fn build_task_graph(&mut self, resources: &Resources) {
        self.task_graph.clear();

        let mut edges = resources
            .packages
            .keys()
            .flat_map(|key| {
                resources.package_deps[resources.packages[key].deps]
                    .iter()
                    .map(move |dep| (dep.ptr, key))
            })
            .collect::<BumpVec<_>>();
        edges.sort_unstable();

        resources
            .packages
            .values()
            .map(|package| resources.package_deps[package.deps].len())
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

    fn sweep_resources(
        &mut self,
        resources: &Resources,
        main_task: &mut Task,
        module_items: &mut Map<VRef<Source>, ModuleItems>,
        to_remove: Vec<VRef<Source>>,
    ) {
        /*
            Overview:
                1.
                    - mark all reachable items
                    - store their dependencies
                2.
                    - find gaps and ordered living items
                    - move outer items into gaps
                    - rewrite their dependencies
                3.
                    - truncate FragMaps, save free segments for later use
        */

        for removed in to_remove {
            module_items.remove(&removed);
        }

        module_items
            .values()
            .flat_map(|items| items.items.values())
            .for_each(|item| self.mark_module_item(item, resources, main_task));
    }

    fn mark_module_item(
        &mut self,
        module_item: &ModuleItem,
        resources: &Resources,
        main_task: &Task,
    ) {
        match module_item.ptr {
            ModuleItemPtr::Func(func) => self.mark_func(func, resources, main_task),
            ModuleItemPtr::Ty(ty) => self.mark_ty(ty, resources, main_task),
            ModuleItemPtr::SpecBase(spec_base) => {
                self.mark_spec_base(spec_base, resources, main_task)
            }
            ModuleItemPtr::Impl(r#impl_base) => self.mark_impl(r#impl_base, resources, main_task),
        };
    }

    fn mark_func(
        &mut self,
        func: FragRef<Func>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.funcs.mark(func)?;
        let Func {
            generics,
            owner,
            upper_generics,
            signature,
            ..
        } = main_task.typec[func];
        self.mark_generics(generics, resources, main_task);
        if let Some(owner) = owner {
            self.mark_ty(owner, resources, main_task);
        }
        self.mark_generics(upper_generics, resources, main_task);
        self.mark_signature(signature, resources, main_task);
        Some(())
    }

    fn mark_ty(&mut self, ty: Ty, resources: &Resources, main_task: &Task) -> Option<()> {
        match ty {
            Ty::Struct(s) => self.mark_struct(s, resources, main_task),
            Ty::Enum(e) => self.mark_enum(e, resources, main_task),
            Ty::Instance(i) => self.mark_instance(i, resources, main_task),
            Ty::Pointer(p) => self.mark_pointer(p, resources, main_task),
            Ty::Param(..) | Ty::Builtin(..) => Some(()),
        }
    }

    fn mark_struct(
        &mut self,
        s: FragRef<Struct>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.structs.mark(s)?;
        let Struct {
            generics, fields, ..
        } = main_task.typec[s];
        self.mark_generics(generics, resources, main_task);
        self.relocator.fields.mark_slice_summed(fields);
        for field in &main_task.typec[fields] {
            self.mark_ty(field.ty, resources, main_task);
        }
        Some(())
    }

    fn mark_enum(
        &mut self,
        e: FragRef<Enum>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.enums.mark(e)?;
        let Enum {
            generics, variants, ..
        } = main_task.typec[e];
        self.mark_generics(generics, resources, main_task);
        self.relocator.variants.mark_slice_summed(variants);
        for variant in &main_task.typec[variants] {
            self.mark_ty(variant.ty, resources, main_task);
        }
        Some(())
    }

    fn mark_instance(
        &mut self,
        i: FragRef<Instance>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.instances.mark(i)?;
        let Instance { base, args } = main_task.typec[i];
        self.mark_ty(base.as_ty(), resources, main_task);
        self.mark_args(args, resources, main_task);
        Some(())
    }

    fn mark_pointer(
        &mut self,
        p: FragRef<Pointer>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.pointers.mark(p)?;
        let Pointer { base, .. } = main_task.typec[p];
        self.mark_ty(base, resources, main_task);
        Some(())
    }

    fn mark_generics(
        &mut self,
        generics: Generics,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.params.mark_slice_summed(generics)?;
        for &spec_sum in &main_task.typec[generics] {
            self.mark_spec_sum(spec_sum, resources, main_task);
        }
        Some(())
    }

    fn mark_args(
        &mut self,
        args: FragSlice<Ty>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.args.mark_slice_summed(args)?;
        for &arg in &main_task.typec[args] {
            self.mark_ty(arg, resources, main_task);
        }
        Some(())
    }

    fn mark_spec_sum(
        &mut self,
        spec_sum: FragSlice<Spec>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.spec_sums.mark_slice_summed(spec_sum)?;
        for &spec in &main_task.typec[spec_sum] {
            self.mark_spec(spec, resources, main_task);
        }
        Some(())
    }

    fn mark_spec(&mut self, spec: Spec, resources: &Resources, main_task: &Task) -> Option<()> {
        match spec {
            Spec::Base(spec_base) => self.mark_spec_base(spec_base, resources, main_task),
            Spec::Instance(spec_instance) => {
                self.mark_spec_instance(spec_instance, resources, main_task)
            }
        }
    }

    fn mark_spec_base(
        &mut self,
        spec_base: FragRef<SpecBase>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.base_specs.mark(spec_base)?;
        let SpecBase {
            generics,
            inherits,
            methods,
            ..
        } = main_task.typec[spec_base];
        self.mark_generics(generics, resources, main_task);
        self.mark_spec_sum(inherits, resources, main_task);
        self.relocator.spec_funcs.mark_slice_summed(methods);
        for &spec_func in &main_task.typec[methods] {
            self.mark_spec_func(spec_func, resources, main_task);
        }
        Some(())
    }

    fn mark_spec_instance(
        &mut self,
        spec_instance: FragRef<SpecInstance>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.spec_instances.mark(spec_instance)?;
        let SpecInstance { base, args } = main_task.typec[spec_instance];
        self.mark_spec_base(base, resources, main_task);
        self.mark_args(args, resources, main_task);
        Some(())
    }

    fn mark_spec_func(
        &mut self,
        SpecFunc {
            generics,
            signature,
            ..
        }: SpecFunc,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.mark_generics(generics, resources, main_task);
        self.mark_signature(signature, resources, main_task);
        Some(())
    }

    fn mark_signature(
        &mut self,
        Signature { args, ret, .. }: Signature,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.mark_args(args, resources, main_task);
        self.mark_ty(ret, resources, main_task);
        Some(())
    }

    fn mark_impl(
        &mut self,
        r#impl: FragRef<Impl>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.impls.mark(r#impl)?;
        let Impl {
            generics,
            key: ImplKey { ty, spec },
            methods,
            ..
        } = main_task.typec[r#impl];
        self.mark_generics(generics, resources, main_task);
        self.mark_ty(ty, resources, main_task);
        self.mark_spec(spec, resources, main_task);
        self.mark_impl_methods(methods, resources, main_task);
        Some(())
    }

    fn mark_impl_methods(
        &mut self,
        methods: FragRefSlice<Func>,
        resources: &Resources,
        main_task: &Task,
    ) -> Option<()> {
        self.relocator.func_slices.mark_slice_summed(methods)?;
        for &method in &main_task.typec[methods] {
            self.mark_func(method, resources, main_task);
        }
        Some(())
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

pub struct WorkerConnections {
    pub package_tasks: Receiver<(PackageTask, VRef<Package>)>,
    pub package_products: Sender<(PackageTask, VRef<Package>)>,
    pub gen_tasks: Receiver<Task>,
}

type ConnsReturn = (
    WorkerConnections,
    SyncSender<(PackageTask, VRef<Package>)>,
    SyncSender<Task>,
);

impl WorkerConnections {
    pub fn new(package_products: Sender<(PackageTask, VRef<Package>)>) -> ConnsReturn {
        let (package_dump, package_tasks) = mpsc::sync_channel(0);
        let (gen_dump, gen_tasks) = mpsc::sync_channel(0);
        (
            Self {
                package_tasks,
                package_products,
                gen_tasks,
            },
            package_dump,
            gen_dump,
        )
    }
}

pub struct Worker {
    pub state: WorkerState,
    pub context: Context,
    pub function_builder_ctx: FunctionBuilderContext,
}

impl Default for Worker {
    fn default() -> Self {
        Self::new()
    }
}

impl Worker {
    pub fn new() -> Self {
        Self {
            state: default(),
            context: Context::new(),
            function_builder_ctx: FunctionBuilderContext::new(),
        }
    }

    pub fn run<'a: 'b, 'b>(
        mut self,
        thread_scope: &'a thread::Scope<'b, '_>,
        shared: Shared<'b>,
        connections: WorkerConnections,
    ) -> ScopedJoinHandle<'b, (Worker, Option<Task>)> {
        thread_scope.spawn(move || {
            let mut arena = Arena::new();
            let mut jit_ctx = JitContext::new(iter::empty());
            self.state.jit_layouts.ptr_ty = shared.jit_isa.pointer_ty;
            self.state.gen_layouts.ptr_ty = shared.isa.pointer_ty;
            loop {
                let Ok((mut package_task, package)) = connections.package_tasks.recv() else {break;};

                let modules = mem::take(&mut package_task.task.modules_to_compile);
                for &module in modules.iter() {
                    self.compile_module(
                        module,
                        &mut arena,
                        &mut package_task.task,
                        &mut jit_ctx,
                        &shared,
                    );
                }
                package_task.task.modules_to_compile = modules;
                connections.package_products
                    .send((package_task, package))
                    .expect("tasks are always reused");
            }

            let Some(mut compile_task) = connections.gen_tasks.recv().ok() else {
                return (self, None);
            };

            let mut gen_layouts = mem::take(&mut self.state.gen_layouts);
            self.compile_current_requests(&mut compile_task, &shared, shared.isa, &mut gen_layouts);
            (self, Some(compile_task))
        })
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
        task.compile_requests.clear();
        self.state.jit_layouts = layouts;

        jit_ctx
            .load_functions(
                compiled.into_iter().chain(imported),
                &task.gen,
                &task.typec,
                &task.interner,
                false,
            )
            .unwrap();
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
            drops,
        } in &task.compile_requests.queue
        {
            compiled.push(id);
            let Func { signature, .. } = task.typec.funcs[func];

            let body = task
                .mir
                .bodies
                .get(&func)
                .expect("every source code function has body")
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
            let root = body
                .blocks
                .keys()
                .next()
                .expect("no block function is invalid");
            self.state.gen_resources.calls.clear();
            self.state
                .gen_resources
                .calls
                .extend(task.compile_requests.children[children].iter().copied());
            self.state.gen_resources.drops.clear();
            for &drop in task.compile_requests.drop_children[drops].iter() {
                let prev = self.state.gen_resources.calls.len();
                self.state
                    .gen_resources
                    .calls
                    .extend(&task.compile_requests.children[drop]);
                self.state
                    .gen_resources
                    .drops
                    .push(prev..self.state.gen_resources.calls.len());
            }
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
                // print!("{} {}", name, self.context.func.display());
            }
            self.context.compile(&*isa.inner).unwrap();
            task.gen.save_compiled_code(id, &self.context).unwrap();
            self.context.clear();
        }
        compiled
    }

    fn push_macro_compile_requests(
        &mut self,
        macros: &[MacroCompileRequest],
        task: &mut Task,
        shared: &Shared,
    ) -> BumpVec<FragRef<CompiledFunc>> {
        let extractor = |&MacroCompileRequest { r#impl, params, .. }| {
            let Impl { methods, .. } = task.typec.impls[r#impl];

            let params = task.typec[params].to_bumpvec();
            // todo try to avoid moving and allocate ty VSlice right away
            let pushed_params = task.compile_requests.ty_slices.bump_slice(&params);

            let collector = |&func| {
                let key = Generator::func_instance_name(
                    true,
                    &shared.jit_isa.triple,
                    func,
                    params.iter().copied(),
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
            name,
            r#impl,
            ty,
            params,
            ..
        } in macros
        {
            let impl_ent = task.typec.impls[r#impl];
            let spec = impl_ent.key.spec.base(&task.typec);

            match spec {
                s if s == SpecBase::TOKEN_MACRO => {
                    if let Some(spec) = self.state.token_macros.get(&r#impl) {
                        let tm = jit_ctx.token_macro(spec).unwrap();
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
            let params = task.typec[params].to_bumpvec();
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
                        .expect("all functions should be present");
                    let tm = jit_ctx
                        .token_macro(&r#macro)
                        .expect("all functions should be present");
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
            let from_param_presence = typec.contains_params_low(from);
            let to_param_presence = typec.contains_params_low(to);
            match from_param_presence.combine(to_param_presence) {
                ParamPresence::Present => {
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
                ParamPresence::BehindPointer => continue,
                ParamPresence::Absent => (),
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

pub struct PackageTask {
    self_sender: SyncSender<(PackageTask, VRef<Package>)>,
    pub task: Task,
}

impl PackageTask {
    pub fn send(
        self,
        package: VRef<Package>,
    ) -> Result<(), mpsc::SendError<(PackageTask, VRef<Package>)>> {
        self.self_sender.clone().send((self, package))
    }

    pub fn into_task(self) -> Task {
        self.task
    }
}

#[derive(Default)]
pub struct GenTask {}

#[derive(Default)]
pub struct Task {
    // config
    pub id: usize,

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
        let mut type_frontier = bumpvec![];
        while let Some((CompileRequestChild { id, func, params }, task_id)) = frontier.pop() {
            let Some(task_id) = task_id else { continue; };
            let task = &mut tasks[task_id];
            let Func {
                flags, visibility, ..
            } = task.typec[func];
            let generics = task
                .typec
                .pack_func_param_specs(func)
                .collect::<BumpVec<_>>();
            if flags.contains(FuncFlags::BUILTIN) {
                continue;
            }
            if visibility == FuncVisibility::Imported {
                imported.push(id);
                continue;
            }

            let body = task.mir.bodies.get(&func).unwrap().clone();
            let mut types = body.types.clone();
            let bumped_params = task.compile_requests.ty_slices[params].to_bumpvec();
            Worker::swap_mir_types(
                &body.inner,
                &mut types,
                &bumped_params,
                &mut task.typec,
                &mut task.interner,
            );

            let mut drops = bumpvec![cap body.drops.len()];
            for (drop, task_id) in body.drops.values().zip(cycle.by_ref()) {
                let task = &mut tasks[task_id];
                let prev = frontier.len();
                type_frontier.push(types[body.values[drop.value].ty].ty);
                while let Some(ty) = type_frontier.pop() {
                    if !task.typec.may_need_drop(ty, &mut task.interner) {
                        continue;
                    }

                    if let Some(Some((r#impl, params))) =
                        ty.is_drop(&generics, &mut task.typec, &mut task.interner)
                    {
                        let func = task.typec[task.typec[r#impl].methods][0];
                        let params = task.typec[params].to_bumpvec();
                        frontier.push(task.load_call(
                            CallableMir::Func(func),
                            params,
                            task_id,
                            isa,
                            &mut seen,
                        ));
                    }

                    match ty {
                        Ty::Struct(s) => {
                            task.typec[task.typec[s].fields]
                                .iter()
                                // we do rev to preserve recursive order of fields
                                .rev()
                                .map(|f| f.ty)
                                .collect_into(&mut *type_frontier);
                        }
                        Ty::Enum(e) => {
                            task.typec[task.typec[e].variants]
                                .iter()
                                .rev()
                                .map(|v| v.ty)
                                .collect_into(&mut *type_frontier);
                        }
                        Ty::Instance(i) => {
                            let Instance { base, args } = task.typec[i];
                            match base {
                                GenericTy::Struct(s) => {
                                    task.typec[task.typec[s].fields]
                                        .to_bumpvec()
                                        .into_iter()
                                        .rev()
                                        .map(|f| {
                                            task.typec.instantiate(f.ty, args, &mut task.interner)
                                        })
                                        .collect_into(&mut *type_frontier);
                                }
                                GenericTy::Enum(e) => {
                                    task.typec[task.typec[e].variants]
                                        .to_bumpvec()
                                        .into_iter()
                                        .rev()
                                        .map(|v| {
                                            task.typec.instantiate(v.ty, args, &mut task.interner)
                                        })
                                        .collect_into(&mut *type_frontier);
                                }
                            }
                        }
                        Ty::Pointer(..) | Ty::Param(..) | Ty::Builtin(..) => (),
                    }
                }
                drops.push(
                    task.compile_requests
                        .children
                        .extend(frontier[prev..].iter().map(|&(child, _)| child)),
                );
            }
            let task = &mut tasks[task_id];
            let drops = task.compile_requests.drop_children.extend(drops);

            let prev = frontier.len();
            body.calls
                .values()
                .zip(cycle.by_ref())
                .map(|(&call, task_id)| {
                    let task = &mut tasks[task_id];
                    let params = body.ty_params[call.params]
                        .iter()
                        .map(|&p| types[p].ty)
                        .collect::<BumpVec<_>>();
                    task.load_call(call.callable, params, task_id, isa, &mut seen)
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
                drops,
            });
        }

        imported
    }

    fn load_call(
        &mut self,
        callable: CallableMir,
        params: BumpVec<Ty>,
        task_id: usize,
        isa: &Isa,
        seen: &mut Map<Ident, FragRef<CompiledFunc>>,
    ) -> (CompileRequestChild, Option<usize>) {
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
        let task_id = matches!(entry, Entry::Vacant(..)).then_some(task_id);
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
        let generic_count = params.len() - generics.len() - 1;
        let (upper, caller) = (&params[..generic_count], params[generic_count]);
        let used_spec = if upper.is_empty() {
            Spec::Base(parent)
        } else {
            Spec::Instance(self.typec.spec_instance(parent, upper, &mut self.interner))
        };

        let (r#impl, params) = self
            .typec
            .find_implementation(caller, used_spec, &[][..], &mut None, &mut self.interner)
            .unwrap()
            .unwrap();
        let func_id = self.typec[self.typec[r#impl].methods][index];
        let params = self.typec[params].to_bumpvec();
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

#[derive(Default)]
pub struct Incremental {
    pub resources: Resources,
    pub main_task: Task,
    pub worker_pool: Vec<Worker>,
    pub module_items: Map<VRef<Source>, ModuleItems>,
}
