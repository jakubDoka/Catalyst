pub mod marking;
pub mod task;
pub mod worker;

use std::{
    collections::VecDeque,
    default::default,
    fmt::Write,
    iter, mem,
    num::NonZeroU8,
    path::*,
    slice,
    sync::mpsc::{self, Receiver, Sender, SyncSender},
    thread::{self, ScopedJoinHandle},
};

use cli::CliInput;
use cranelift_codegen::{
    ir::{self, InstBuilder},
    isa::CallConv,
    Context,
};
use cranelift_frontend::FunctionBuilderContext;
use snippet_display::annotate_snippets::display_list::FormatOptions;

use crate::*;

#[derive(Default)]
pub struct Middleware {
    pub workspace: Workspace,
    pub(crate) package_graph: PackageGraph,
    pub(crate) resource_loading_ctx: ResourceLoaderCtx,
    pub(crate) task_graph: TaskGraph,
    pub(crate) entry_points: Vec<FragRef<Func>>,
    pub(crate) incremental: Option<Incremental>,
    pub(crate) relocator: TypecRelocator,
    pub(crate) gen_relocator: FragRelocator<CompiledFunc>,
}

impl Middleware {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn take_incremental(&mut self) -> Option<Incremental> {
        self.incremental.take()
    }

    fn reload_resources(
        &mut self,
        resources: &mut Resources,
        main_task: &mut Task,
        db: &mut dyn ResourceDb,
        path: &Path,
    ) -> Option<Vec<VRef<Source>>> {
        PackageLoader::new(
            resources,
            &mut main_task.workspace,
            &mut main_task.interner,
            &mut self.package_graph,
            db,
        )
        .reload(path, &mut self.resource_loading_ctx)
    }

    pub fn update(
        &mut self,
        args: &MiddlewareArgs,
        db: &mut dyn ResourceDb,
    ) -> (MiddlewareOutput, DiagnosticView) {
        self.workspace.clear();

        let Incremental {
            mut resources,
            mut main_task,
            mut worker_pool,
            mut module_items,
        } = self.incremental.take().unwrap_or_else(|| {
            let mut incr = Incremental::default();
            incr.main_task.typec.init(&mut incr.main_task.interner);
            incr
        });

        main_task.ir_dump = args.dump_ir.then(String::new);

        if let Some(removed) = self.reload_resources(&mut resources, &mut main_task, db, &args.path)
        {
            self.relocator.clear();
            self.gen_relocator.clear();
            self.sweep_resources(&resources, &mut main_task, &mut module_items, removed);
            main_task.mir.relocate(&self.relocator);
            main_task.gen.retain_incremental(&self.relocator.funcs);
        };

        if main_task.workspace.has_errors() || resources.no_changes() {
            let output = if resources.no_changes() {
                MiddlewareOutput::Unchanged
            } else {
                MiddlewareOutput::Failed
            };

            self.workspace.transfer(&mut main_task.workspace);
            let incr = self.incremental.insert(Incremental {
                resources,
                main_task,
                worker_pool,
                module_items,
            });

            return (
                output,
                DiagnosticView {
                    workspace: &mut self.workspace,
                    resources: &incr.resources,
                },
            );
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

        self.task_graph.prepare(&resources);

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

            let mut tasks = self.expand(
                package_receiver,
                package_tasks,
                &resources,
                workers_count,
                &mut module_items,
            );

            for task in tasks.iter_mut() {
                self.workspace.transfer(&mut task.workspace);
            }

            if self.workspace.has_errors() || args.check {
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

        if self.workspace.has_errors() || args.check {
            let incr = self.incremental.insert(Incremental {
                resources,
                main_task,
                worker_pool,
                module_items,
            });

            let output = if args.check {
                MiddlewareOutput::Checked
            } else {
                MiddlewareOutput::Failed
            };

            return (
                output,
                DiagnosticView {
                    workspace: &mut self.workspace,
                    resources: &incr.resources,
                },
            );
        }

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
            .map_err(|err| {
                if let ObjectRelocationError::MissingSymbol(id) = err {
                    let func = main_task.gen[id].func;
                    dbg!(&main_task.interner[main_task.typec[func].name]);
                }
            })
            .unwrap();

        let ir = main_task.ir_dump.take();

        main_task.gen.reallocate(&mut self.gen_relocator);
        self.workspace.transfer(&mut main_task.workspace);
        let incr = self.incremental.insert(Incremental {
            resources,
            main_task,
            worker_pool,
            module_items,
        });

        (
            MiddlewareOutput::Compiled {
                binary: object.emit().unwrap(),
                ir,
            },
            DiagnosticView {
                workspace: &mut self.workspace,
                resources: &incr.resources,
            },
        )
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
        let name = main_task.interner.intern(gen::ENTRY_POINT_NAME);
        let entry_point = main_task
            .gen
            .get_or_insert(name, || CompiledFunc::new(func_id, name));

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
            let id = tasks[task_id]
                .gen
                .get_or_insert(key, || CompiledFunc::new(func, key));
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
        cached_items: &mut Map<VRef<Source>, ModuleItems>,
    ) -> Vec<Task> {
        let mut module_items = ShadowMap::new();

        for (key, module) in resources.modules.iter() {
            if let Some(items) = cached_items.remove(&module.source) {
                module_items[key] = items;
            }
        }

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
                    .filter(|&&module| {
                        resources.modules[module].package == package
                            && resources.sources[resources.modules[module].source].changed
                    })
                    .collect_into(&mut package_task.task.modules_to_compile);

                if package_task.task.modules_to_compile.is_empty() {
                    self.task_graph.finish(package);
                    tasks.push(package_task);
                    continue;
                }

                sync_module_items(&mut package_task.task.typec.module_items, &module_items);
                package_task
                    .send(package)
                    .expect("worker thread terminated prematurely");
            }

            if tasks.len() == worker_count {
                assert!(!self.task_graph.has_tasks());
                break;
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
            .map(|(task, ..)| task.task)
            .inspect(|task| sync_module_items(&mut module_items, &task.typec.module_items));
        let res = tasks
            .into_iter()
            .map(|task| task.task)
            .chain(leftover_tasks)
            .collect();

        for (key, items) in module_items.iter_mut() {
            cached_items.insert(resources.modules[key].source, mem::take(items));
        }

        res
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
    pub check: bool,
}

impl MiddlewareArgs {
    pub fn from_cli_input(cli_input: &CliInput) -> Result<Self, IsaCreationError> {
        let isa = match cli_input.value("target") {
            Some(_triple) => todo!(),
            None => Isa::host(false)?,
        };

        Ok(MiddlewareArgs {
            path: cli_input
                .args()
                .get(1)
                .cloned()
                .unwrap_or(".".into())
                .into(),
            jit_isa: Isa::host(true)?,
            isa,
            incremental_path: cli_input.value("incremental-path").map(|s| s.into()),
            max_cores: cli_input.value("max-cores").and_then(|s| s.parse().ok()),
            dump_ir: cli_input.enabled("dump-ir"),
            check: cli_input.enabled("check"),
        })
    }
}

pub enum MiddlewareOutput {
    Compiled { binary: Vec<u8>, ir: Option<String> },
    Checked,
    Unchanged,
    Failed,
}

const CAST_HINT: &str = "if you know what you are doing, perform cast trough pointers";

ctl_errors! {
    #[err => "cast between generic types is not allowed"]
    #[info => "cast from '{from}' to '{to}', which contain generic parameters that depend on function instance"]
    #[note => CAST_HINT]
    error CastBetweenGenericTypes: fatal {
        #[err loc]
        from ref : String,
        to ref: String,
        loc: SourceLoc,
    }

    #[err => "cast between types of different size"]
    #[info => "cast from '{from}'({from_size}) to '{to}'({to_size})"]
    #[note => CAST_HINT]
    error CastSizeMismatch: fatal {
        #[err loc]
        from ref: String,
        from_size: u32,
        to ref: String,
        to_size: u32,
        loc: SourceLoc,
    }

    #[err => "cast to type with greater align"]
    #[info => "cast from '{from}'({from_align}) to '{to}'({to_align})"]
    #[note => CAST_HINT]
    error CastAlignMismatch: fatal {
        #[err loc]
        from ref: String,
        from_align: NonZeroU8,
        to ref: String,
        to_align: NonZeroU8,
        loc: SourceLoc,
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
pub struct Incremental {
    pub resources: Resources,
    pub main_task: Task,
    pub worker_pool: Vec<Worker>,
    pub module_items: Map<VRef<Source>, ModuleItems>,
}

pub struct DiagnosticView<'a> {
    pub workspace: &'a mut Workspace,
    pub resources: &'a Resources,
}

impl<'a> DiagnosticView<'a> {
    pub fn changed_files(&self) -> impl Iterator<Item = &Path> {
        self.resources
            .sources
            .values()
            .filter_map(|s| s.changed.then_some(s.path.as_path()))
    }

    pub fn dump_diagnostics(
        &self,
        color: bool,
        output: MiddlewareOutput,
    ) -> Result<(Vec<u8>, Option<String>), String> {
        Err(match output {
            MiddlewareOutput::Compiled { binary, ir } => return Ok((binary, ir)),
            MiddlewareOutput::Checked => "No errors found.".into(),
            MiddlewareOutput::Unchanged => "No changes detected.".into(),
            MiddlewareOutput::Failed => {
                let mut display = SnippetDisplayImpl {
                    opts: FormatOptions { color, ..default() },
                    ..default()
                };

                let mut diagnostics = String::new();
                self.workspace
                    .display(self.resources, &mut display, &mut diagnostics);

                diagnostics + "\n\n Compilation failed."
            }
        })
    }
}

fn swap_mir_types(
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
