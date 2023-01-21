pub mod task;
pub mod worker;

use core::fmt;
use std::{
    collections::VecDeque,
    default::default,
    env,
    error::Error,
    fmt::Write,
    fs,
    io::BufWriter,
    iter, mem,
    num::NonZeroU8,
    os::unix::prelude::MetadataExt,
    path::*,
    slice,
    sync::mpsc::{self, Receiver, Sender, SyncSender},
    thread::{self, ScopedJoinHandle},
};

use bytecheck::CheckBytes;

use cli::CliInput;
use cranelift_codegen::{
    ir::{self, InstBuilder},
    Context,
};
use cranelift_frontend::FunctionBuilderContext;
use rkyv::{
    check_archived_root,
    de::{deserializers::SharedDeserializeMap, SharedDeserializeRegistry},
    ser::{
        serializers::{AllocScratch, CompositeSerializer, SharedSerializeMap, WriteSerializer},
        ScratchSpace, Serializer, SharedSerializeRegistry,
    },
    validation::validators::DefaultValidator,
    with::{AsStringError, Skip, UnixTimestampError},
    Archive, Deserialize, Fallible, Serialize,
};
use snippet_display::annotate_snippets::display_list::FormatOptions;

use crate::*;

use self::{task::TaskBase, worker::DefaultSourceAstHandler};

pub type WorkerLaunchResult<'scope> = (
    Vec<PackageTask>,
    Vec<SyncSender<Task>>,
    Vec<ScopedJoinHandle<'scope, (Worker, Option<Task>)>>,
);

#[derive(Default)]
pub struct Middleware {
    pub workspace: Workspace,
    pub(crate) package_graph: PackageGraph,
    pub(crate) resource_loading_ctx: ResourceLoaderCtx,
    pub(crate) task_graph: TaskGraph,
    pub(crate) entry_points: Vec<FragRef<Func>>,
    pub(crate) incremental: Option<Incremental>,
    pub(crate) relocator: FragRelocator,
    save_path: Option<PathBuf>,
}

impl Middleware {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn save(&mut self, save_path: Option<PathBuf>) -> Result<(), Box<dyn Error>> {
        let Some(path) = save_path.or(self.save_path.take()) else {
            return Ok(());
        };

        let Some(prefix) = path.parent() else {
            return Err("empty path".into())
        };
        std::fs::create_dir_all(prefix)?;

        let file = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .open(&path)?;
        let writer = BufWriter::new(file);

        let mut serializer = MiddlewareSerializer(CompositeSerializer::new(
            WriteSerializer::new(writer),
            AllocScratch::new(),
            SharedSerializeMap::default(),
        ));
        serializer.serialize_value(
            self.incremental
                .as_ref()
                .ok_or_else(|| "incremental data is not present")?,
        )?;

        Ok(())
    }

    pub fn from_bytes<'a, T>(bytes: &'a [u8]) -> Result<T, Box<dyn Error + Send + Sync>>
    where
        T: Archive,
        T::Archived: 'a
            + CheckBytes<DefaultValidator<'a>>
            + Deserialize<T, MiddlewareDeserializer<SharedDeserializeMap>>,
    {
        Ok(check_archived_root::<'a, T>(bytes)
            .map_err(|e| format!("{e}"))?
            .deserialize(&mut MiddlewareDeserializer(SharedDeserializeMap::default()))?)
    }

    pub fn load(&mut self, incremental: &[u8]) -> Result<(), Box<dyn Error + Send + Sync>> {
        let incr = Self::from_bytes::<Incremental>(incremental)?;
        if incr.exe_mod_time.is_none() || get_exe_mod_time() != incr.exe_mod_time {
            return Err("data is outdated".into());
        }

        self.incremental = Some(incr);

        Ok(())
    }

    pub fn take_incremental(&mut self) -> Option<Incremental> {
        self.incremental.take()
    }

    fn reload_resources(
        &mut self,
        resources: &mut Resources,
        base: &mut TaskBase,
        db: &mut dyn ResourceDb,
        path: &Path,
    ) -> Option<Vec<VRef<Source>>> {
        // Since split is iterator we create just one instance for this single threaded use_case
        let mut interner = base
            .interner
            .split()
            .next()
            .expect("we already clamped threads between 1 and 255");
        let changed = PackageLoader::new(
            resources,
            &mut self.workspace,
            &mut interner,
            &mut self.package_graph,
            db,
        )
        .reload(path, &mut self.resource_loading_ctx);
        changed
    }

    pub fn traverse_source_ast<S: AstHandler>(
        &mut self,
        args: &MiddlewareArgs,
        handlers: &mut [S],
    ) -> Option<DiagnosticView> {
        if handlers.is_empty() {
            return None;
        }

        let Some(Incremental {
            resources,
            task_base,
            mut worker_pool,
            module_items,
            builtin_functions,
            exe_mod_time,
        }) = self.incremental.take() else {
            return None;
        };

        let (package_sender, package_receiver) = mpsc::channel();
        drop(package_receiver);

        let mut tasks = task_base
            .split(false)
            .take(handlers.len())
            .collect::<Vec<_>>();

        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
            builtin_functions: &builtin_functions,
        };

        let mut arena = Arena::new();
        for ((key, package), i) in resources.packages.iter().zip((0..handlers.len()).cycle()) {
            let task = &mut tasks[i];
            let handler = &mut handlers[i];
            if handler.should_skip_manifest(key, &resources) {
                continue;
            }
            let source = &resources.sources[package.source].content;
            let mut parser_ctx = ParserCtx::new(source);
            arena.clear();
            let Some(manifest) = handler.parse_manifest(Parser::new(
                &mut task.interner,
                &mut self.workspace,
                &mut parser_ctx,
                &arena,
                package.source,
                source,
            )) else {
                continue;
            };
            handler.manifest(manifest, package.source, &resources);
        }

        let Some(dummy_package) = resources.packages.keys().next() else {
            self.incremental = Some(Incremental {
                resources,
                task_base,
                worker_pool,
                module_items,
                builtin_functions,
                exe_mod_time,
            });
            return None;
        };

        Self::distribute_modules(&mut tasks, &resources);

        thread::scope(|scope| {
            let (package_tasks, gen_senders, threads) = Self::launch_workers(
                scope,
                package_sender,
                tasks,
                handlers,
                &mut worker_pool,
                shared,
            );
            drop(gen_senders);

            for task in package_tasks.into_iter() {
                task.send(dummy_package)
                    .expect("channel should not be dead right now");
            }

            Self::join_workers(threads, &mut worker_pool);
        });

        let incr = self.incremental.insert(Incremental {
            resources,
            task_base,
            worker_pool,
            module_items,
            builtin_functions,
            exe_mod_time,
        });

        Some(DiagnosticView {
            workspace: &mut self.workspace,
            resources: &incr.resources,
        })
    }

    fn distribute_modules(tasks: &mut [Task], resources: &Resources) {
        tasks
            .iter_mut()
            .for_each(|task| task.modules_to_compile.clear());

        for (key, id) in resources.modules.keys().zip((0..tasks.len()).cycle()) {
            tasks[id].modules_to_compile.push(key);
        }
    }

    fn load_on_update(&mut self, path: &Option<PathBuf>) {
        if self.incremental.is_some() {
            return;
        }

        let Some(ref path) = path else {return};

        self.save_path = Some(path.clone());

        let Ok(bytes) = fs::read(path) else {return};

        let Err(err) = self.load(&bytes) else {return};

        self.workspace.push(IncrementalDataIssue { err });
    }

    pub fn update(
        &mut self,
        args: &MiddlewareArgs,
        db: &mut dyn ResourceDb,
    ) -> (MiddlewareOutput, DiagnosticView) {
        self.workspace.clear();

        self.load_on_update(&args.incremental_path);

        let thread_count = args.thread_count();

        let reloaded = self.incremental.is_some();

        let Incremental {
            mut resources,
            mut task_base,
            mut worker_pool,
            mut module_items,
            mut builtin_functions,
            exe_mod_time,
        } = self
            .incremental
            .take()
            .unwrap_or_else(|| Incremental::new(thread_count.max(255) as u8));

        if reloaded {
            resources
                .sources
                .values_mut()
                .for_each(|s| s.changed = false);
        }

        if let Some(removed) = self.reload_resources(&mut resources, &mut task_base, db, &args.path)
            && !removed.is_empty()
        {
            self.sweep_resources(&mut task_base, &mut module_items, removed, thread_count, &mut builtin_functions);
        };

        if self.workspace.has_errors() || resources.no_changes() {
            let output = if self.workspace.has_errors() {
                MiddlewareOutput::Failed
            } else {
                MiddlewareOutput::Unchanged
            };

            let incr = self.incremental.insert(Incremental {
                resources,
                task_base,
                worker_pool,
                module_items,
                builtin_functions,
                exe_mod_time,
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

        // if thread_count == 1 {
        //     // todo!();
        // }

        self.task_graph.prepare(&resources);

        let tasks = task_base.split(args.dump_ir).collect::<Vec<_>>();
        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
            builtin_functions: &builtin_functions,
        };

        let mut handlers = vec![DefaultSourceAstHandler; thread_count];

        let (tasks, entry_points, imported) = thread::scope(|scope| {
            let (package_tasks, mut gen_senders, threads) = Self::launch_workers(
                scope,
                package_sender,
                tasks,
                &mut handlers,
                &mut worker_pool,
                shared,
            );

            let mut tasks = self.expand(
                &mut task_base,
                package_receiver,
                package_tasks,
                &resources,
                thread_count,
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

            task_base.gen.prepare();

            let (entry_points, imported) = self.distribute_compile_requests(&mut tasks, &args.isa);
            for (task, recv) in tasks.into_iter().zip(gen_senders.iter_mut()) {
                recv.send(task).expect("worker terminated prematurely");
            }

            (
                Self::join_workers(threads, &mut worker_pool),
                entry_points,
                imported,
            )
        });

        let (ir, mut main_task, compiled) = self.tidy_tasks(&mut task_base, tasks);

        if self.workspace.has_errors() || args.check {
            let incr = self.incremental.insert(Incremental {
                resources,
                task_base,
                worker_pool,
                module_items,
                builtin_functions,
                exe_mod_time,
            });

            let output = if self.workspace.has_errors() {
                MiddlewareOutput::Failed
            } else {
                MiddlewareOutput::Checked
            };

            return (
                output,
                DiagnosticView {
                    workspace: &mut self.workspace,
                    resources: &incr.resources,
                },
            );
        }

        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
            builtin_functions: &builtin_functions,
        };

        let entry_point = worker_pool
            .first_mut()
            .expect("since there is nonzero cores, there has to be worker")
            .generaite_entry_point(&mut main_task, &args.isa, &shared, &entry_points);

        let mut object = ObjectContext::new(&args.isa).unwrap();
        object
            .load_functions(
                compiled
                    .into_iter()
                    .chain(imported)
                    .chain(iter::once(entry_point)),
                &main_task.gen,
                &main_task.typec,
                &main_task.interner,
            )
            // .map_err(|err| {
            //     if let ObjectRelocationError::MissingSymbol(id) = err {
            //         let func = main_task.gen[id].func;
            //         dbg !(&main_task.interner[main_task.typec[func].name]);
            //     }
            // })
            .unwrap();

        let incr = self.incremental.insert(Incremental {
            resources,
            task_base,
            worker_pool,
            module_items,
            builtin_functions,
            exe_mod_time,
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

    fn join_workers(
        threads: Vec<ScopedJoinHandle<(Worker, Option<Task>)>>,
        worker_pool: &mut Vec<Worker>,
    ) -> Vec<Task> {
        threads
            .into_iter()
            .map(|thread| {
                let (worker, task) = thread.join().expect("worker panicked");
                worker_pool.push(worker);
                task.expect("impossible since gen_senders still hasn't been dropped")
            })
            .collect()
    }

    fn tidy_tasks(
        &mut self,
        base: &mut TaskBase,
        mut tasks: Vec<Task>,
    ) -> (Option<String>, Task, Vec<FragRef<CompiledFunc>>) {
        // we want consistent output during tests
        tasks.sort_unstable_by_key(|t| t.id);

        for task in tasks.iter_mut() {
            self.workspace.transfer(&mut task.workspace);
            task.commit(base);
        }

        let ir = tasks
            .iter_mut()
            .filter_map(|task| task.ir_dump.take())
            .reduce(|a, b| a + &b);

        let compiled = tasks
            .iter_mut()
            .flat_map(|task| task.compile_requests.queue.drain(..))
            .map(|req| req.id)
            .collect();

        let mut main_task = tasks.pop().expect("has to be at least one task");
        main_task.pull(base);

        (ir, main_task, compiled)
    }

    fn launch_workers<'a: 'scope, 'scope, S: AstHandler>(
        scope: &'a thread::Scope<'scope, '_>,
        package_sender: Sender<(PackageTask, VRef<Package>)>,
        tasks: Vec<Task>,
        handlers: &'a mut [S],
        worker_pool: &mut Vec<Worker>,
        shared: Shared<'scope>,
    ) -> WorkerLaunchResult<'scope> {
        let worker_count = tasks.len();
        let workers = iter::repeat_with(|| WorkerConnections::new(package_sender.clone()))
            .take(worker_count)
            .collect::<Vec<_>>();

        let (mut package_tasks, mut gen_senders, mut threads) = (
            Vec::with_capacity(worker_count),
            Vec::with_capacity(worker_count),
            Vec::with_capacity(worker_count),
        );

        for (((connections, package_sender, gen_sender), task), handler) in
            workers.into_iter().zip(tasks).zip(handlers)
        {
            package_tasks.push(PackageTask {
                self_sender: package_sender,
                task,
            });
            gen_senders.push(gen_sender);
            threads.push(worker_pool.pop().unwrap_or_default().run(
                scope,
                shared,
                connections,
                handler,
            ));
        }

        (package_tasks, gen_senders, threads)
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
        let distributor = |(func, task_id): (_, usize)| {
            let main_task = &mut tasks[0];
            let key = Generator::func_instance_name(
                isa.jit,
                &isa.triple,
                func,
                iter::empty(),
                &main_task.typec,
                &mut main_task.interner,
            );
            let id = tasks[task_id].gen.get_or_insert(key, func);
            (
                CompileRequestChild {
                    id,
                    func,
                    params: default(),
                },
                Ok(task_id),
            )
        };
        let frontier = self
            .entry_points
            .drain(..)
            .zip(task_distribution)
            .map(distributor)
            .collect::<BumpVec<_>>();

        let internal = frontier.iter().map(|(req, _)| req.id).collect();
        let external = Task::traverse_compile_requests(frontier, tasks, isa);
        (internal, external.to_vec())
    }

    pub fn expand(
        &mut self,
        task_base: &mut TaskBase,
        receiver: Receiver<(PackageTask, VRef<Package>)>,
        tasks: Vec<PackageTask>,
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

        let mut tasks = VecDeque::from(tasks);

        while self.task_graph.has_tasks() {
            // we start with front and push back to equally distribute entities
            // we also want first task to initialize the water drops
            while let Some(mut package_task) = tasks.pop_front() {
                let Some(package) = self.task_graph.next_task() else {
                    tasks.push_front(package_task);
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
                    tasks.push_front(package_task);
                    continue;
                }

                sync_module_items(&mut package_task.task.typec.module_items, &module_items);
                package_task.task.pull(task_base);
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
                package_task.task.commit(task_base);
                self.task_graph.finish(package);
                tasks.push_back(package_task);

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
            .map(|(task, ..)| task);
        let mut res = tasks
            .into_iter()
            .chain(leftover_tasks)
            .map(|task: PackageTask| {
                sync_module_items(&mut module_items, &task.task.typec.module_items);
                task.task
            })
            .collect::<Vec<_>>();

        for task in res.iter_mut() {
            task.commit(task_base);
        }
        for task in res.iter_mut() {
            task.pull(task_base);
        }

        for (key, items) in module_items.iter_mut() {
            cached_items.insert(resources.modules[key].source, mem::take(items));
        }

        res
    }

    fn sweep_resources(
        &mut self,
        task_base: &mut TaskBase,
        module_items: &mut Map<VRef<Source>, ModuleItems>,
        removed: Vec<VRef<Source>>,
        thread_count: usize,
        mut builtin_functions: &mut [FragRef<Func>],
    ) {
        for source in removed {
            module_items.remove(&source);
        }

        let mut threads = iter::repeat_with(Vec::new)
            .take(thread_count)
            .collect::<Vec<_>>();

        for (item, thread) in module_items
            .values_mut()
            .flat_map(|val| val.items.values_mut())
            .zip((0..thread_count).cycle())
        {
            threads[thread].push(item);
        }

        let mut frags = RelocatedObjects::default();
        frags.add_root(&mut builtin_functions);
        frags.add_static_root(&Func::ALL);
        frags.add_static_root(&Enum::ALL);
        frags.add_static_root(&Struct::ALL);
        frags.add_static_root(&SpecBase::ALL);
        task_base.register(&mut frags);
        self.relocator.relocate(&mut threads, &mut frags);
    }
}

impl Drop for Middleware {
    fn drop(&mut self) {
        if let Err(e) = self.save(None) {
            eprintln!("Save error: {e}")
        }
    }
}

#[derive(Clone, Copy)]
pub struct Shared<'a> {
    pub resources: &'a Resources,
    pub jit_isa: &'a Isa,
    pub isa: &'a Isa,
    pub builtin_functions: &'a [FragRef<Func>],
}

pub struct CommandInfo<'a> {
    pub general: &'a str,
    pub flags: &'a [(&'a str, &'a str)],
    pub values: &'a [(&'a str, &'a str, &'a str)],
    pub inherit: Option<&'a CommandInfo<'a>>,
}

impl<'a> fmt::Display for CommandInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.general)?;
        f.write_char('\n')?;
        f.write_str("FLAGS:\n")?;
        let flag_max_len = self
            .flags
            .iter()
            .map(|(name, ..)| name.len())
            .max()
            .unwrap_or(0);
        for (name, desc) in self.flags {
            let spacing = " ".repeat(flag_max_len - name.len());
            writeln!(f, "\t-{name}{spacing} - {desc}")?;
        }

        f.write_str("VALUES:\n")?;
        let value_max_len = self
            .flags
            .iter()
            .map(|(name, ..)| name.len())
            .max()
            .unwrap_or(0);
        let type_max_len = self
            .flags
            .iter()
            .map(|(name, ..)| name.len())
            .max()
            .unwrap_or(0);
        for (name, value, desc) in self.values {
            let value_spacing = " ".repeat(value_max_len - name.len());
            let type_spacing = " ".repeat(type_max_len - name.len());
            writeln!(
                f,
                "\t--{name}{value_spacing} {value}{type_spacing} - {desc}"
            )?;
        }

        if let Some(inherit) = self.inherit {
            write!(f, "\n{}", inherit)?;
        }

        Ok(())
    }
}

#[macro_export]
macro_rules! command_info {
    (
        $name:ident [$($general:tt)*] $(: $inherit:expr;)?
        flags { $($flags:tt)* }
        values { $($values:tt)* }
    ) => {
        pub const $name: $crate::CommandInfo<'static> = $crate::CommandInfo {
            general: concat!($($general)*),
            flags: $crate::command_info!(@flags $($flags)*),
            values: $crate::command_info!(@values $($values)*),
            inherit: $crate::command_info!(@inherit $($inherit)?),
        };
    };

    (@inherit $inherit:expr) => { Some(&$inherit) };
    (@inherit) => { None };

    (@flags $($name:literal => $desc:literal)*) => {
        &[$(($name, $desc)),*]
    };

    (@values $($name:literal($type:literal) => $desc:literal)*) => {
        &[$(($name, $type, $desc)),*]
    };
}

pub struct MiddlewareArgs {
    pub path: PathBuf,
    pub jit_isa: Isa,
    pub isa: Isa,
    pub incremental_path: Option<PathBuf>,
    pub max_cores: Option<usize>,
    pub dump_ir: bool,
    pub check: bool,
    pub quiet: bool,
}

impl MiddlewareArgs {
    command_info! {
        HELP
        ["ctl <subcommand> [path='.'] [-flag...] [--key <value>...]"]
        flags {
            "help" => "this message"
            "target" => "target triple of final binary"
            "check" => "skip codegen"
            "quiet" => "don't print anything"
        }
        values {
            "max-cores"("N") => "maximum cores used during compilation"
            "incremental-path"("string") => "where the incremental data should be stored"
        }
    }

    pub fn thread_count(&self) -> usize {
        thread::available_parallelism()
            .map_or(1, |n| n.get())
            .min(self.max_cores.unwrap_or(usize::MAX))
    }

    pub fn from_cli_input(cli_input: &CliInput, help: CommandInfo) -> Result<Self, Box<dyn Error>> {
        if cli_input.enabled("help") {
            Err(help.to_string())?;
        }

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
            incremental_path: cli_input
                .value("incremental-path")
                .or_else(|| Some("incremental/default.rkyv"))
                .filter(|_| !cli_input.enabled("no-incremental"))
                .map(|s| s.into()),
            max_cores: cli_input.value("max-cores").and_then(|s| s.parse().ok()),
            dump_ir: cli_input.enabled("dump-ir"),
            check: cli_input.enabled("check"),
            quiet: cli_input.enabled("quiet"),
        })
    }

    pub fn display(&self) -> MiddlewareArgsDisplay {
        MiddlewareArgsDisplay { quiet: self.quiet }
    }
}

pub struct MiddlewareArgsDisplay {
    quiet: bool,
}

impl fmt::Write for MiddlewareArgsDisplay {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.quiet {
            return Ok(());
        }

        eprintln!("{}", s);

        Ok(())
    }
}

#[derive(Debug)]
pub enum MiddlewareOutput {
    Compiled { binary: Vec<u8>, ir: Option<String> },
    Checked,
    Unchanged,
    Failed,
}

impl MiddlewareOutput {
    pub fn is_failed(&self) -> bool {
        matches!(self, Self::Failed)
    }
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

    #[info => "failed to load incremental data"]
    #[info => "trace: {err}"]
    error IncrementalDataIssue {
        err ref: Box<dyn Error + Send + Sync>
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

#[derive(Serialize, Deserialize, Archive)]
#[archive_attr(derive(CheckBytes))]
pub struct Incremental {
    pub exe_mod_time: Option<i64>,
    pub resources: Resources,
    pub task_base: TaskBase,
    #[with(Skip)]
    pub worker_pool: Vec<Worker>,
    pub module_items: Map<VRef<Source>, ModuleItems>,
    pub builtin_functions: Vec<FragRef<Func>>,
}

fn get_exe_mod_time() -> Option<i64> {
    let path = env::current_exe().ok()?;
    let metadata = fs::metadata(path).ok()?;
    Some(metadata.mtime())
}

impl Incremental {
    pub fn new(thread_count: u8) -> Self {
        let mut builtin_functions = vec![];
        Incremental {
            exe_mod_time: get_exe_mod_time(),
            resources: default(),
            task_base: TaskBase::new(thread_count, &mut builtin_functions),
            worker_pool: default(),
            module_items: default(),
            builtin_functions,
        }
    }
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
    generics: VRefSlice<MirTy>,
    module: &ModuleMirInner,
    dependant_types: &mut FuncTypes,
    params: &[Ty],
    typec: &mut Typec,
    interner: &mut Interner,
) {
    if params.is_empty() {
        return;
    }

    for &mir_ty in &module.ty_params[generics] {
        let ty = dependant_types[mir_ty].ty;
        let new_ty = typec.instantiate(ty, params, interner);
        dependant_types[mir_ty].ty = new_ty;
    }
}

pub struct MiddlewareSerializer<T: ?Sized>(T);

impl<I: Serializer + ?Sized> Serializer for MiddlewareSerializer<I> {
    fn pos(&self) -> usize {
        self.0.pos()
    }

    fn write(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        self.0
            .write(bytes)
            .map_err(MiddlewareSerializerError::Inner)
    }

    fn pad(&mut self, padding: usize) -> Result<(), Self::Error> {
        self.0
            .pad(padding)
            .map_err(MiddlewareSerializerError::Inner)
    }

    fn align(&mut self, align: usize) -> Result<usize, Self::Error> {
        self.0
            .align(align)
            .map_err(MiddlewareSerializerError::Inner)
    }

    fn align_for<T>(&mut self) -> Result<usize, Self::Error> {
        self.0
            .align_for::<T>()
            .map_err(MiddlewareSerializerError::Inner)
    }

    unsafe fn resolve_aligned<T: Archive + ?Sized>(
        &mut self,
        value: &T,
        resolver: T::Resolver,
    ) -> Result<usize, Self::Error> {
        self.0
            .resolve_aligned(value, resolver)
            .map_err(MiddlewareSerializerError::Inner)
    }

    unsafe fn resolve_unsized_aligned<T: rkyv::ArchiveUnsized + ?Sized>(
        &mut self,
        value: &T,
        to: usize,
        metadata_resolver: T::MetadataResolver,
    ) -> Result<usize, Self::Error> {
        self.0
            .resolve_unsized_aligned(value, to, metadata_resolver)
            .map_err(MiddlewareSerializerError::Inner)
    }
}

impl<T: ScratchSpace> ScratchSpace for MiddlewareSerializer<T> {
    unsafe fn push_scratch(
        &mut self,
        layout: std::alloc::Layout,
    ) -> Result<std::ptr::NonNull<[u8]>, Self::Error> {
        self.0
            .push_scratch(layout)
            .map_err(MiddlewareSerializerError::Inner)
    }

    unsafe fn pop_scratch(
        &mut self,
        ptr: std::ptr::NonNull<u8>,
        layout: std::alloc::Layout,
    ) -> Result<(), Self::Error> {
        self.0
            .pop_scratch(ptr, layout)
            .map_err(MiddlewareSerializerError::Inner)
    }
}

impl<T: SharedSerializeRegistry> SharedSerializeRegistry for MiddlewareSerializer<T> {
    fn get_shared_ptr(&self, value: *const u8) -> Option<usize> {
        self.0.get_shared_ptr(value)
    }

    fn add_shared_ptr(&mut self, value: *const u8, pos: usize) -> Result<(), Self::Error> {
        self.0
            .add_shared_ptr(value, pos)
            .map_err(MiddlewareSerializerError::Inner)
    }
}

impl<T: Fallible + ?Sized> Fallible for MiddlewareSerializer<T> {
    type Error = MiddlewareSerializerError<T::Error>;
}

#[derive(Debug)]
pub enum MiddlewareSerializerError<R> {
    Inner(R),
    UnixTimestamp(UnixTimestampError),
    AsString(AsStringError),
    NonMax(NonMaxError),
}

impl<R> From<UnixTimestampError> for MiddlewareSerializerError<R> {
    fn from(value: UnixTimestampError) -> Self {
        Self::UnixTimestamp(value)
    }
}

impl<R> From<AsStringError> for MiddlewareSerializerError<R> {
    fn from(value: AsStringError) -> Self {
        Self::AsString(value)
    }
}

impl<R> From<NonMaxError> for MiddlewareSerializerError<R> {
    fn from(value: NonMaxError) -> Self {
        Self::NonMax(value)
    }
}

impl<R: fmt::Display> fmt::Display for MiddlewareSerializerError<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MiddlewareSerializerError::Inner(inner) => write!(f, "{}", inner),
            MiddlewareSerializerError::UnixTimestamp(u) => write!(f, "{}", u),
            MiddlewareSerializerError::AsString(s) => write!(f, "{}", s),
            MiddlewareSerializerError::NonMax(..) => write!(f, "Non max integer is at MAX"),
        }
    }
}

impl<R: fmt::Display + fmt::Debug> Error for MiddlewareSerializerError<R> {}

pub struct MiddlewareDeserializer<T>(T);

impl<I: Fallible> Fallible for MiddlewareDeserializer<I> {
    type Error = MiddlewareSerializerError<I::Error>;
}

impl<I: SharedDeserializeRegistry> SharedDeserializeRegistry for MiddlewareDeserializer<I> {
    fn get_shared_ptr(&mut self, ptr: *const u8) -> Option<&dyn rkyv::de::SharedPointer> {
        self.0.get_shared_ptr(ptr)
    }

    fn add_shared_ptr(
        &mut self,
        ptr: *const u8,
        shared: Box<dyn rkyv::de::SharedPointer>,
    ) -> Result<(), Self::Error> {
        self.0
            .add_shared_ptr(ptr, shared)
            .map_err(MiddlewareSerializerError::Inner)
    }
}
