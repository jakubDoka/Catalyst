use std::{
    collections::VecDeque,
    default::default,
    fmt::Write,
    iter, mem,
    path::*,
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

// use crate::{incremental::SweepProjections, *};

// macro_rules! sweep {
//     ($self:expr, $source:expr => $target:expr) => {
//         IncrementalBorrow {
//             typec: &mut $source.typec,
//             mir: &mut $source.mir,
//             gen: &mut $source.gen,
//         }
//         .sweep(
//             &$self.sweep_bitset,
//             &mut SweepCtx {
//                 temp: IncrementalBorrow {
//                     typec: &mut $target.typec,
//                     mir: &mut $target.mir,
//                     gen: &mut $target.gen,
//                 },
//                 projections: &mut $self.sweep_ctx,
//             },
//             &mut InternerTransfer::new(&mut $source.interner, &mut $target.interner),
//         );
//     };
// }

#[derive(Default)]
pub struct Middleware {
    pub workspace: Workspace,
    pub resources: Resources,
    pub package_graph: PackageGraph,
    pub resource_loading_ctx: PackageLoaderCtx,
    pub main_task: Task,
    // pub temp_incremental: Incremental,
    // pub sweep_ctx: SweepProjections,
    pub sweep_bitset: BitSet,
    pub task_graph: TaskGraph,
    pub entry_points: Vec<FragRef<Func>>,
}

impl Middleware {
    pub fn new() -> Self {
        Self::default()
    }

    fn reload_resources(&mut self, path: &Path) {
        PackageLoader::new(
            &mut self.resources,
            &mut self.workspace,
            &mut self.main_task.interner,
            &mut self.package_graph,
        )
        .reload(path, &mut self.resource_loading_ctx);
    }

    pub fn update(&mut self, args: &MiddlewareArgs) -> Option<MiddlewareOutput> {
        let mut ir = args.dump_ir.then(String::new);

        // if let Some(ref path) = args.incremental_path {
        //     self.prepare_incremental(path, &args.path);
        // } else {
        self.reload_resources(&args.path);
        // }

        if self.workspace.has_errors() {
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

        let binary = thread::scope(|s| {
            let mut senders = workers
                .into_iter()
                .enumerate()
                .map(|(id, (worker, sender))| {
                    let shared = Shared {
                        resources: &resources,
                        jit_isa: &args.jit_isa,
                        isa: &args.isa,
                    };
                    worker.run(s, shared);
                    (
                        sender,
                        Some(Task {
                            id,
                            ir_dump: ir.clone(),
                            ..Task::default()
                        }),
                    )
                })
                .collect::<Vec<_>>();

            let mut tasks = self.expand(&mut receiver, &mut senders, &resources);
            self.distribute_compile_requests(&mut tasks, &args.isa);
            for (task, (recv, ..)) in tasks.into_iter().zip(senders.iter_mut()) {
                recv.send(task).expect("Bruh...");
            }

            let to_link = (0..num_workers)
                .flat_map(|_| {
                    let mut task = receiver.recv().expect("Eh...");
                    if let (Some(ir), Some(dump)) = (&mut ir, task.ir_dump) {
                        ir.push_str(&dump);
                        ir.push('\n');
                    }
                    task.workspace.transfer(&mut self.workspace);
                    task.compile_requests.queue.into_iter().map(move |req| {
                        let (key, value) = task.gen.funcs.remove_index(req.id);
                        (req.id, key, value)
                    })
                })
                .map(|(id, key, value)| {
                    self.main_task.gen.funcs.insert(key, value);
                    id
                })
                .collect::<BumpVec<_>>();

            let entry_point = self.generate_entry_point(&args.isa);

            let mut object = ObjectContext::new(&args.isa).expect("This really sucks...");
            object
                .load_functions(
                    to_link.into_iter().chain(iter::once(entry_point)),
                    &self.main_task.gen,
                    &self.main_task.typec,
                    &self.main_task.interner,
                )
                .expect("So close...");
            object.emit().expect("This is so sad...")
        });

        self.resources = resources;

        Some(MiddlewareOutput { binary, ir })
    }

    fn generate_entry_point(&mut self, isa: &Isa) -> FragRef<CompiledFunc> {
        let mut context = Context::new();
        let mut func_ctx = FunctionBuilderContext::new();

        let (body, dependant) = (default(), default());
        let mut builder = GenBuilder::new(isa, &body, &mut context.func, &dependant, &mut func_ctx);

        let entry_point = builder.create_block();
        builder.append_block_params_for_function_params(entry_point);
        builder.switch_to_block(entry_point);

        for (index, func) in self.main_task.gen.funcs.indexed_values() {
            if !self.main_task.typec.funcs[func.func]
                .flags
                .contains(FuncFlags::ENTRY)
            {
                continue;
            }

            let signature = ir::Signature::new(CallConv::Fast);
            let sig_ref = builder.import_signature(signature);
            let external_name = ir::UserExternalName::new(0, index.as_u32());
            let name = builder.func.declare_imported_user_function(external_name);
            let func_ref = builder.import_function(ir::ExtFuncData {
                name: ir::ExternalName::user(name),
                signature: sig_ref,
                colocated: true,
            });
            builder.ins().call(func_ref, &[]);
        }
        builder.ins().return_(&[]);

        let func_id = self.main_task.typec.funcs.push(Func {
            visibility: FuncVisibility::Exported,
            name: self.main_task.interner.intern(gen::ENTRY_POINT_NAME),
            ..default()
        });
        let entry_point = self.main_task.gen.funcs.insert_unique(
            self.main_task.interner.intern(gen::ENTRY_POINT_NAME),
            CompiledFunc::new(func_id),
        );

        context.compile(&*isa.inner).expect("Real nice...");
        self.main_task
            .gen
            .save_compiled_code(entry_point, &context)
            .expect("Not again...");

        entry_point
    }

    pub fn distribute_compile_requests(&mut self, tasks: &mut [Task], isa: &Isa) {
        let frontier = self
            .entry_points
            .iter()
            .map(|&func| {
                let key = Generator::func_instance_name(
                    false,
                    &isa.triple,
                    func,
                    iter::empty(),
                    &self.main_task.typec,
                    &mut self.main_task.interner,
                );
                let id = self
                    .main_task
                    .gen
                    .funcs
                    .insert_unique(key, CompiledFunc::new(func));
                let _ = self.main_task.gen.funcs[id].clone();

                CompileRequestChild {
                    id,
                    func,
                    params: default(),
                }
            })
            .collect();

        let mut compile_requests = CompileRequests::default();
        Worker::traverse_compile_requests(
            frontier,
            &self.main_task.mir,
            &mut self.main_task.gen,
            &mut self.main_task.typec,
            &mut self.main_task.interner,
            &mut compile_requests,
            isa,
        );

        for task in tasks.iter_mut() {
            task.for_generation = true;
            Self::transfer_all(task, &self.main_task);
        }

        let chunk_size =
            ((compile_requests.queue.len() & !(tasks.len() - 1)) + tasks.len()) / tasks.len();
        let chunks = compile_requests.queue.chunks(chunk_size);

        for (task, chunk) in tasks.iter_mut().zip(chunks) {
            for req in chunk {
                let key = self.main_task.gen.funcs.id(req.id);
                let value = self.main_task.gen.funcs[req.id].clone();
                task.gen.funcs.insert(key, value);
            }
            task.compile_requests.queue.extend(chunk);
            task.compile_requests
                .ty_slices
                .clone_from(&compile_requests.ty_slices);
            task.compile_requests
                .children
                .clone_from(&compile_requests.children);
        }
    }

    pub fn expand(
        &mut self,
        receiver: &mut Receiver<Task>,
        senders: &mut [(SyncSender<Task>, Option<Task>)],
        resources: &Resources,
    ) -> Vec<Task> {
        while self.task_graph.has_tasks() {
            for (sender, maybe_task) in senders.iter_mut() {
                let Some(mut task) = maybe_task.take() else {
                    continue;
                };

                let Some(package) = self.task_graph.next_task() else {
                    *maybe_task = Some(task);
                    break;
                };

                dbg!(package);

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
                sender.send(task).expect("Whoops...");
            }

            let mut task = receiver.recv().expect("Very bad...");
            loop {
                self.entry_points.append(dbg!(&mut task.entry_points));
                self.sweep_bitset.clear();
                for &module in &task.modules_to_compile {
                    self.sweep_bitset.insert(module.index());
                }
                dbg!("start");
                sweep!(self, task => self.incremental);
                dbg!("end");
                let id = task.id;
                senders[id].1 = Some(task);
                if let Ok(next_task) = receiver.try_recv() {
                    task = next_task;
                } else if !self.task_graph.has_tasks()
                    && senders.iter().any(|(_, task)| task.is_none())
                {
                    task = receiver.recv().expect("Interesting...");
                } else {
                    break;
                }
            }
        }
        dbg!("done");

        senders
            .iter_mut()
            .map(|(.., task)| {
                task.take()
                    .unwrap_or_else(|| receiver.recv().expect("We are doomed!"))
            })
            .collect::<Vec<_>>()
    }

    pub fn transfer_all(task: &mut Task, incremental: &Task) {
        task.typec.transfer(&incremental.typec);
        task.interner.clone_from(&incremental.interner);
        task.mir.bodies.clone_from(&incremental.mir.bodies);
        task.gen.funcs.clone_from(&incremental.gen.funcs);
    }

    // pub fn prepare_incremental(&mut self, path: &Path, resource_path: &Path) {
    //     if let Err(err) = self.load_incremental(path) {
    //         self.workspace.push(snippet! {
    //             warn: ("Failed to load incremental data: {}", err);
    //             info: "Rebuilding from scratch";
    //             info: ("queried path: {}", path.display());
    //         })
    //     };

    //     self.resources.sources = mem::take(&mut self.incremental.sources);

    //     self.reload_resources(resource_path);
    //     self.sweep_bitset.clear();
    //     for (key, module) in self.resources.modules.iter() {
    //         if !self.resources.sources[module.source].changed {
    //             self.sweep_bitset.insert(key.index());
    //         }
    //     }

    //     sweep!(self, self.incremental => self.temp_incremental);
    //     mem::swap(&mut self.incremental, &mut self.temp_incremental);
    //     self.temp_incremental.clear();
    //     self.incremental.loaded = true;
    // }

    // fn load_incremental(&mut self, path: &Path) -> std::io::Result<()> {
    //     if self.incremental.loaded || !path.exists() {
    //         return Ok(());
    //     }

    //     let file = std::fs::File::open(path)?;
    //     let mut buffered = std::io::BufReader::new(file);
    //     self.incremental = rmp_serde::from_read(&mut buffered)
    //         .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;

    //     Ok(())
    // }

    // pub fn save(&mut self, path: &Path) -> std::io::Result<()> {
    //     let file = std::fs::File::create(path)?;
    //     let mut buffered = std::io::BufWriter::new(file);
    //     self.incremental.sources = mem::take(&mut self.resources.sources);
    //     rmp_serde::encode::write(&mut buffered, &self.incremental)
    //         .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;

    //     Ok(())
    // }

    pub fn build_task_graph(&mut self) {
        self.task_graph.clear();

        let mut edges = self
            .resources
            .packages
            .keys()
            .flat_map(|key| {
                dbg!(&self.resources.sources[self.resources.packages[key].source].path);
                self.resources.package_deps[self.resources.packages[key].deps]
                    .iter()
                    .map(move |dep| (dep.ptr, key))
            })
            .collect::<BumpVec<_>>();
        edges.sort_unstable();

        dbg!(&edges);

        self.task_graph.meta.extend(
            self.resources
                .packages
                .values()
                .map(|package| self.resources.package_deps[package.deps].len())
                .map(|count| (count, default())),
        );

        for package_deps in edges.group_by(|(a, _), (b, _)| *a == *b) {
            let package = package_deps[0].0;

            let (count, ref mut children) = self.task_graph.meta[package.index()];
            *children = self
                .task_graph
                .inverse_graph
                .bump(package_deps.iter().map(|&(_, dep)| dep));
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
    pub fn next_task(&mut self) -> Option<VRef<Package>> {
        let current = self.frontier.pop_front();

        if let Some(current) = current {
            let (.., deps) = self.meta[current.index()];
            for &dep in &self.inverse_graph[deps] {
                let index = dep.index();
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
                dbg!(task.for_generation);
                if task.for_generation {
                    break task;
                }

                let modules = mem::take(&mut task.modules_to_compile);
                for &module in modules.iter() {
                    self.compile_module(module, &mut arena, &mut task, &mut jit_ctx, &shared);
                }
                task.modules_to_compile = modules;
                self.products.send(task).expect("As I was saying...");
                dbg!("Sent task");
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
    ) -> BumpVec<FragRef<CompiledFunc>> {
        let mut compiled = bumpvec![];
        for &CompileRequest {
            func,
            id,
            params,
            children,
        } in &task.compile_requests.queue
        {
            let Func { signature, .. } = task.typec.funcs[func];
            let body = &task.mir.bodies[func].inner;
            let params = task.compile_requests.ty_slices[params].to_bumpvec();
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
            self.state.gen_resources.calls.clear();
            self.state
                .gen_resources
                .calls
                .extend(task.compile_requests.children[children].iter().copied());
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
                write!(dump, "{}", self.context.func.display()).unwrap();
            }
            self.context.compile(&*isa.inner).expect("Failure!");
            task.gen
                .save_compiled_code(id, &self.context)
                .expect("Superb error occurred!");
            compiled.push(id);
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
    ) {
        for &MacroCompileRequest { ty, r#impl, .. } in macros {
            let Impl {
                generics,
                methods,
                key: ImplKey { ty: template, .. },
                ..
            } = task.typec.impls[r#impl];

            let mut params = bumpvec![None; generics.len()];
            task.typec
                .compatible(&mut params, ty, template)
                .expect("Heh...");
            let params = params
                .into_iter()
                .collect::<Option<BumpVec<_>>>()
                .expect("Lovely!");
            let pushed_params = task.compile_requests.ty_slices.bump_slice(&params);
            let frontier = task.typec.func_slices[methods]
                .iter()
                .map(|&func| {
                    let key = Generator::func_instance_name(
                        true,
                        &shared.jit_isa.triple,
                        func,
                        params.iter().cloned(),
                        &task.typec,
                        &mut task.interner,
                    );
                    let id = task.gen.funcs.insert_unique(key, CompiledFunc::new(func));
                    CompileRequestChild {
                        func,
                        id,
                        params: pushed_params,
                    }
                })
                .collect::<BumpVec<_>>();

            Self::traverse_compile_requests(
                frontier,
                &task.mir,
                &mut task.gen,
                &mut task.typec,
                &mut task.interner,
                &mut task.compile_requests,
                shared.jit_isa,
            );
        }
    }

    fn traverse_compile_requests(
        mut frontier: BumpVec<CompileRequestChild>,
        mir: &Mir,
        gen: &mut Gen,
        typec: &mut Typec,
        interner: &mut Interner,
        compile_requests: &mut CompileRequests,
        isa: &Isa,
    ) {
        while let Some(CompileRequestChild { id, func, params }) = frontier.pop() {
            let body = &mir.bodies[func];
            let mut types = body
                .dependant_types
                .values()
                .copied()
                .collect::<BumpVec<_>>();
            let bumped_params = compile_requests.ty_slices[params].to_bumpvec();
            Worker::swap_mir_types(&body.inner, &mut types, &bumped_params, typec, interner);
            let prev = frontier.len();
            for &CallMir {
                callable, params, ..
            } in body.calls.values()
            {
                let params = body.ty_params[params]
                    .iter()
                    .map(|&ty| body.dependant_types[ty].ty)
                    .collect::<BumpVec<_>>();
                let (func_id, params) = match callable {
                    CallableMir::Func(func_id) => (func_id, params),
                    CallableMir::SpecFunc(func) => {
                        // TODO: I dislike how much can panic here, maybe improve this in the future
                        let SpecFunc {
                            parent, generics, ..
                        } = typec.spec_funcs[func];
                        let SpecBase { methods, .. } = typec[parent];
                        let index = typec.spec_funcs.local_index(methods, func);
                        let generic_count = params.len() - typec[generics].len();
                        let (upper, caller, lower) = (
                            &params[..generic_count - 1],
                            params[generic_count - 1],
                            &params[generic_count..],
                        );
                        let used_spec = if upper.is_empty() {
                            Spec::Base(parent)
                        } else {
                            Spec::Instance(typec.spec_instance(parent, upper, interner))
                        };
                        let r#impl = typec
                            .find_implementation(caller, used_spec, &[], &mut None, interner)
                            .unwrap()
                            .unwrap();
                        let Impl {
                            generics,
                            methods,
                            key: ImplKey { ty, spec },
                            ..
                        } = typec.impls[r#impl];
                        let func_id = typec.func_slices[methods][index];
                        let mut infer_slots = bumpvec![None; generics.len()];
                        let _ = typec.compatible(&mut infer_slots, caller, ty);
                        let _ = typec.compatible_spec(&mut infer_slots, used_spec, spec);
                        let params = infer_slots
                            .iter()
                            .map(|&slot| slot.unwrap())
                            .chain(lower.iter().copied())
                            .collect::<BumpVec<_>>();
                        (func_id, params)
                    }
                    CallableMir::Pointer(_) => todo!(),
                };

                let key = Generator::func_instance_name(
                    false,
                    &isa.triple,
                    func_id,
                    params.iter().cloned(),
                    typec,
                    interner,
                );
                let (id, dup) = gen.funcs.insert(key, CompiledFunc::new(func_id));
                if dup.is_some() {
                    continue;
                }
                let _ = gen.funcs[dbg!(id)].clone();
                frontier.push(CompileRequestChild {
                    func: func_id,
                    id,
                    params: compile_requests.ty_slices.bump_slice(&params),
                });
            }
            let children = compile_requests.children.bump_slice(&frontier[prev..]);
            let _ = gen.funcs[dbg!(id)].clone();
            compile_requests.queue.push(CompileRequest {
                func,
                id,
                params,
                children,
            });
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
                .filter_map(|name| task.gen.funcs.index(name));

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
    fn split(&self) -> Self {
        Self {
            interner: self.interner.clone(),
            typec: self.typec.clone(),
            mir: self.mir.clone(),
            gen: self.gen.clone(),

            ..default()
        }
    }

    fn sync_with(&mut self, other: &Self) {
        for (&key, value) in other.gen.funcs.iter() {
            self.gen.funcs.insert(key, value.clone());
        }
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
    pub temp_dependant_types: Vec<MirTy>,
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
