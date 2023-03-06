use rkyv::{
    de::deserializers::SharedDeserializeMapError,
    ser::serializers::{AllocScratchError, CompositeSerializerError, SharedSerializeMapError},
};

use self::{
    task::{CompileRequestCollector, CompileRequestCollectorCtx, TaskGraph},
    worker::{GeneratorThread, Worker, WorkerConnections},
};

pub mod task;
pub mod worker;

use {
    crate::*,
    cli::CliInput,
    core::fmt,
    cranelift_codegen::{
        ir::{self, InstBuilder},
        settings::{self, Configurable, SetError},
        Context,
    },
    cranelift_frontend::FunctionBuilderContext,
    rkyv::{
        de::{deserializers::SharedDeserializeMap, SharedDeserializeRegistry},
        ser::{
            serializers::{AllocScratch, CompositeSerializer, SharedSerializeMap, WriteSerializer},
            ScratchSpace, Serializer, SharedSerializeRegistry,
        },
        with::{AsStringError, Skip, UnixTimestampError},
        Archive, Deserialize, Fallible, Serialize,
    },
    snippet_display::annotate_snippets::display_list::FormatOptions,
    std::{
        collections::VecDeque,
        default::default,
        env,
        error::Error,
        fmt::Write as FmWrite,
        fs,
        io::{BufWriter, Seek, SeekFrom, Write},
        iter, mem,
        num::NonZeroU8,
        os::unix::prelude::MetadataExt,
        path::*,
        str::FromStr,
        sync::{
            atomic::AtomicBool,
            mpsc::{self, Receiver, Sender, SyncSender},
        },
        thread::{self, ScopedJoinHandle},
        time::Instant,
    },
    target_lexicon::Triple,
    task::TaskBase,
    worker::DefaultSourceAstHandler,
};

type WorkerLaunchResult<'scope> = (Vec<PackageTask>, Vec<ScopedJoinHandle<'scope, Worker>>);

compose_error!(MiddlewareLoadError {
    #["check sequence is corrupted"]
    CorruptedCheckSequence,
    #["data is outdated"]
    OutdatedData,
    #["failed to deserialize incremental blob: {e}"]
    Deserialize(e: MiddlewareSerializerError<SharedDeserializeMapError>),
});

compose_error!(MiddlewareSaveError {
    #["empty path"]
    EmptyPath,
    #["dir creation failed: {e}"]
    DirCreation(e: std::io::Error),
    #["faild (create and)open file for incremental blob: {e}"]
    FileOpen(e: std::io::Error),
    #["failed to perform write operation: {e}"]
    Write(e: std::io::Error),
    #["missing incremental data"]
    MissingData,
    #["failed to serialize incremental data: {e}"]
    Serialization(e: MiddlewareSerializerError<CompositeSerializerError<
        std::io::Error, AllocScratchError, SharedSerializeMapError>>),
});

#[derive(Default)]
pub struct Middleware {
    workspace: Workspace,
    package_graph: PackageGraph,
    resource_loading_ctx: ResourceLoaderCtx,
    task_graph: TaskGraph,
    entry_points: Vec<FragRef<Func>>,
    incremental: Option<Incremental>,
    relocator: FragRelocator,
    gen_relocator: GenRelocator,
    requests: CompileRequests,
    request_ctx: CompileRequestCollectorCtx,
    save_path: Option<PathBuf>,
}

impl Middleware {
    fn save(&mut self, save_path: Option<PathBuf>) -> Result<(), MiddlewareSaveError> {
        let _t = QuickTimer::new("incremental data save");
        let Some(path) = save_path.or(self.save_path.take()) else {
            return Ok(());
        };

        let Some(prefix) = path.parent() else {
            return Err(MiddlewareSaveError::EmptyPath);
        };
        std::fs::create_dir_all(prefix).map_err(MiddlewareSaveError::DirCreation)?;

        let mut file = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .open(&path)
            .map_err(MiddlewareSaveError::FileOpen)?;
        let mut writer = BufWriter::new(&mut file);
        writer
            .write_all(&[0; mem::size_of::<i64>()])
            .map_err(MiddlewareSaveError::Write)?;

        let mut serializer = MiddlewareSerializer(CompositeSerializer::new(
            WriteSerializer::new(writer),
            AllocScratch::new(),
            SharedSerializeMap::default(),
        ));
        serializer
            .serialize_value(
                self.incremental
                    .as_ref()
                    .ok_or(MiddlewareSaveError::MissingData)?,
            )
            .map_err(MiddlewareSaveError::Serialization)?;

        let Some(exe_mod_time) = get_exe_mod_time() else {
            return Ok(());
        };

        drop(serializer);

        file.seek(SeekFrom::Start(0))
            .map_err(MiddlewareSaveError::Write)?;
        file.write_all(&exe_mod_time.to_ne_bytes())
            .map_err(MiddlewareSaveError::Write)?;

        Ok(())
    }

    unsafe fn from_bytes<'a, T>(
        bytes: &'a [u8],
    ) -> Result<T, MiddlewareSerializerError<SharedDeserializeMapError>>
    where
        T: Archive,
        T::Archived: 'a + Deserialize<T, MiddlewareDeserializer<SharedDeserializeMap>>,
    {
        rkyv::util::archived_root::<T>(bytes)
            .deserialize(&mut MiddlewareDeserializer(SharedDeserializeMap::default()))
    }

    fn load(&mut self, incremental: &[u8]) -> Result<(), MiddlewareLoadError> {
        let _t = QuickTimer::new("incremental data load");

        let check_size = mem::size_of::<i64>();
        let Some(check_array) = incremental.get(..check_size).and_then(|slice| slice.try_into().ok()) else {
            return Err(MiddlewareLoadError::CorruptedCheckSequence);
        };

        let check = i64::from_ne_bytes(check_array);
        if get_exe_mod_time() != Some(check) {
            return Err(MiddlewareLoadError::OutdatedData);
        }

        // SAFETY: After former checks, there is nothing more we can do or are willing to do
        // user is just unlucky at this point
        let incr = unsafe {
            Self::from_bytes::<Incremental>(&incremental[check_size..])
                .map_err(MiddlewareLoadError::Deserialize)?
        };

        self.incremental = Some(incr);

        Ok(())
    }

    pub fn unwrap_view(&mut self) -> DiagnosticView {
        DiagnosticView {
            workspace: &mut self.workspace,
            resources: &self.incremental.as_ref().unwrap().resources,
        }
    }

    fn reload_resources(
        &mut self,
        resources: &mut Resources,
        base: &mut TaskBase,
        db: &mut dyn ResourceDb,
        path: &Path,
    ) -> Option<Vec<VRef<Source>>> {
        let _t = QuickTimer::new("(re)loading source files");
        // Since split is iterator we create just one instance for this single threaded use_case
        let mut interner = base
            .interner
            .split()
            .next()
            .expect("we already clamped threads between 1 and 255");

        PackageLoader {
            resources,
            workspace: &mut self.workspace,
            interner: &mut interner,
            package_graph: &mut self.package_graph,
            db,
        }
        .reload(path, &mut self.resource_loading_ctx)
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
            mut task_base,
            mut worker_pool,
            module_items,
            builtin_functions,
        }) = self.incremental.take() else {
            return None;
        };

        let (package_sender, package_receiver) = mpsc::channel();
        drop(package_receiver);

        let mut tasks = task_base
            .split(Some(args))
            .take(handlers.len())
            .collect::<Vec<_>>();

        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
            builtin_functions: &builtin_functions,
        };

        let mut arena = Arena::default();
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
            });
            return None;
        };

        Self::distribute_modules(&mut tasks, &resources);

        thread::scope(|scope| {
            let (package_tasks, threads) = Self::launch_workers(
                scope,
                package_sender,
                tasks,
                handlers,
                &mut worker_pool,
                shared,
            );

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

        let _ = fs::remove_file(path);
        self.workspace.push(IncrementalDataIssue { err });
    }

    pub fn update(
        &mut self,
        args: &MiddlewareArgs,
        db: &mut dyn ResourceDb,
    ) -> (MiddlewareOutput, DiagnosticView) {
        self.workspace.clear();
        self.entry_points.clear();

        self.load_on_update(&args.incremental_path);

        let thread_count = args.thread_count().min(255) as u8;

        let reloaded = self.incremental.is_some();

        let Incremental {
            mut resources,
            mut task_base,
            mut worker_pool,
            mut module_items,
            mut builtin_functions,
        } = self
            .incremental
            .take()
            .unwrap_or_else(|| Incremental::new(thread_count));

        if reloaded {
            task_base.expand(thread_count);
            resources
                .sources
                .values_mut()
                .for_each(|s| s.changed = false);
        }

        if let Some(removed) = self.reload_resources(&mut resources, &mut task_base, db, &args.path)
            && !removed.is_empty()
        {
            Self::sweep_resources(
                &mut self.relocator,
                &mut self.gen_relocator,
                &mut task_base,
                &mut module_items,
                removed,
                thread_count,
                &mut builtin_functions,
            );
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

        let tasks = task_base.split(Some(args)).collect::<Vec<_>>();
        let shared = Shared {
            resources: &resources,
            jit_isa: &args.jit_isa,
            isa: &args.isa,
            builtin_functions: &builtin_functions,
        };

        let mut handlers = vec![DefaultSourceAstHandler; thread_count as usize];

        let mut tasks = thread::scope(|scope| {
            let (package_tasks, threads) = Self::launch_workers(
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

            Self::join_workers(threads, &mut worker_pool);
            tasks
        });

        if self.workspace.has_errors() || args.check {
            let ([_, mir, tir], _) = self.tidy_tasks(&mut task_base, tasks);
            let incr = self.incremental.insert(Incremental {
                resources,
                task_base,
                worker_pool,
                module_items,
                builtin_functions,
            });

            let output = if self.workspace.has_errors() {
                MiddlewareOutput::Failed
            } else {
                MiddlewareOutput::Checked { mir, tir }
            };

            return (
                output,
                DiagnosticView {
                    workspace: &mut self.workspace,
                    resources: &incr.resources,
                },
            );
        }

        let [entry_points, imported] = self.codegen(
            &mut task_base,
            args,
            &mut tasks,
            &mut worker_pool,
            &resources,
        );

        let ([ir, mir, tir], mut main_task) = self.tidy_tasks(&mut task_base, tasks);
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
                self.requests
                    .queue
                    .drain(..)
                    .map(|r| r.id)
                    .chain(imported)
                    .chain(iter::once(entry_point)),
                &main_task.gen,
                &main_task.types,
                &mut main_task.interner,
            )
            .map_err(|err| {
                if let ObjectRelocationError::MissingSymbol(id) = err {
                    dbg!(main_task.interner.get(id.ident()));
                }
            })
            .unwrap();

        let incr = self.incremental.insert(Incremental {
            resources,
            task_base,
            worker_pool,
            module_items,
            builtin_functions,
        });

        (
            MiddlewareOutput::Compiled {
                binary: object.emit().unwrap(),
                ir,
                mir,
                tir,
            },
            DiagnosticView {
                workspace: &mut self.workspace,
                resources: &incr.resources,
            },
        )
    }

    fn join_workers(threads: Vec<ScopedJoinHandle<Worker>>, worker_pool: &mut Vec<Worker>) {
        threads
            .into_iter()
            .map(|thread| thread.join().expect("worker panicked"))
            .collect_into(worker_pool);
    }

    fn tidy_tasks(
        &mut self,
        base: &mut TaskBase,
        mut tasks: Vec<Task>,
    ) -> ([Option<String>; 3], Task) {
        // we want consistent output during tests
        tasks.sort_unstable_by_key(|t| t.id);

        for task in tasks.iter_mut() {
            self.workspace.transfer(&mut task.workspace);
            task.commit(base);
        }

        let dumps = tasks
            .iter_mut()
            .map(|task| {
                [
                    task.ir_dump.take(),
                    task.mir_dump.take(),
                    task.tir_dump.take(),
                ]
            })
            .reduce(|a, b| a.zip(b).map(|(a, b)| a.and_then(|a| b.map(|b| a + &b))))
            .unwrap_or([None, None, None]);

        let mut main_task = tasks.pop().expect("has to be at least one task");
        main_task.pull(base);

        (dumps, main_task)
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
        let (connections, input): (Vec<_>, Vec<_>) = (0..worker_count)
            .map(|_| WorkerConnections::new(package_sender.clone()))
            .unzip();

        let package_tasks = tasks
            .into_iter()
            .zip(input)
            .map(|(task, self_sender)| PackageTask { task, self_sender })
            .collect();
        let threads = worker_pool
            .drain(..)
            .chain(iter::repeat_with(Worker::default))
            .zip(connections)
            .zip(handlers)
            .map(|((worker, connection), handler)| worker.run(scope, shared, connection, handler))
            .collect();

        (package_tasks, threads)
    }

    fn distribute_compile_requests(
        roots: impl IntoIterator<Item = FragRef<Func>>,
        requests: &mut CompileRequests,
        request_cxt: &mut CompileRequestCollectorCtx,
        task: &mut Task,
        isa: &Isa,
    ) -> (
        Vec<CompiledFuncRef>, // in executable
        Vec<CompiledFuncRef>, // in lib or dll
    ) {
        let distributor = |func| {
            let key = Generator::func_instance_name(
                isa.jit,
                &isa.triple,
                func,
                iter::empty(),
                &task.types,
                &mut task.interner,
            );
            let (id, ..) = task.gen.get_or_insert_func(key, func);
            CompileRequestChild {
                id,
                func,
                params: default(),
            }
        };
        let frontier = roots.into_iter().map(distributor).collect::<BumpVec<_>>();

        let internal = frontier.iter().map(|req| req.id).collect();
        let external = CompileRequestCollector {
            requests,
            isa,
            types: &mut task.types,
            interner: &mut task.interner,
            gen: &mut task.gen,
            ctx: request_cxt,
            mir: &mut task.mir,
        }
        .collect(frontier);
        (internal, external.to_vec())
    }

    fn expand(
        &mut self,
        task_base: &mut TaskBase,
        receiver: Receiver<(PackageTask, VRef<Package>)>,
        tasks: Vec<PackageTask>,
        resources: &Resources,
        worker_count: u8,
        cached_items: &mut Map<VRef<Source>, ModuleItems>,
    ) -> Vec<Task> {
        let _t = QuickTimer::new("parsing, type checking, macro expansion");
        let mut module_items = ShadowMap::new();

        for (key, module) in resources.modules.iter() {
            let key = resources.modules[key].source;
            if let Some(items) = cached_items.remove(&module.source) {
                module_items[key] = items;
            }
        }

        fn sync_module_items(
            target: &mut ShadowMap<Source, ModuleItems>,
            source: &ShadowMap<Source, ModuleItems>,
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

                sync_module_items(&mut package_task.task.types.module_items, &module_items);
                package_task.task.pull(task_base);
                package_task
                    .send(package)
                    .expect("worker thread terminated prematurely");
            }

            if tasks.len() == worker_count as usize {
                assert!(!self.task_graph.has_tasks());
                break;
            }

            let (mut package_task, mut package) = receiver
                .recv()
                .expect("All worker threads terminated prematurely");
            loop {
                self.entry_points
                    .append(&mut package_task.task.entry_points);
                sync_module_items(&mut module_items, &package_task.task.types.module_items);
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
            .take(worker_count as usize - tasks.len())
            .map(|(task, ..)| task);
        let mut res = tasks
            .into_iter()
            .chain(leftover_tasks)
            .map(|task: PackageTask| {
                sync_module_items(&mut module_items, &task.task.types.module_items);
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
            cached_items.insert(key, mem::take(items));
        }

        res
    }

    fn sweep_resources(
        relocator: &mut FragRelocator,
        gen_relocator: &mut GenRelocator,
        task_base: &mut TaskBase,
        module_items: &mut Map<VRef<Source>, ModuleItems>,
        removed: Vec<VRef<Source>>,
        thread_count: u8,
        mut builtin_functions: &mut [FragRef<Func>],
    ) {
        let _t = QuickTimer::new("resource clear");

        for source in removed {
            module_items.remove(&source);
        }

        let mut threads = iter::repeat_with(Vec::new)
            .take(thread_count as usize)
            .collect::<Vec<_>>();

        for (item, thread) in module_items
            .values_mut()
            .flat_map(|val| val.items.values_mut())
            .zip((0..thread_count).cycle())
        {
            threads[thread as usize].push(item);
        }

        let mut frags = RelocatedObjects::default();
        frags.add_root(&mut builtin_functions);
        frags.add_static_root(&Func::ALL);
        frags.add_static_root(&Enum::ALL);
        frags.add_static_root(&Struct::ALL);
        frags.add_static_root(&SpecBase::ALL);
        task_base.register(&mut frags, gen_relocator);
        relocator.relocate(&mut threads, &mut frags);
    }

    fn codegen(
        &mut self,
        task_base: &mut TaskBase,
        args: &MiddlewareArgs,
        tasks: &mut [Task],
        worker_pool: &mut [Worker],
        resources: &Resources,
    ) -> [Vec<CompiledFuncRef>; 2] {
        let _t = QuickTimer::new("codegen");
        let thread_count = tasks.len() as u8;
        task_base.gen.prepare();

        let (generated, imported) = Self::distribute_compile_requests(
            self.entry_points.drain(..),
            &mut self.requests,
            &mut self.request_ctx,
            tasks.first_mut().expect("what are we doing then"),
            &args.isa,
        );

        thread::scope(|scope| {
            for ((task, requests), worker) in tasks
                .iter_mut()
                .zip(self.requests.split(thread_count))
                .zip(worker_pool.iter_mut())
            {
                scope.spawn(|| {
                    GeneratorThread {
                        requests,
                        mir: &mut task.mir,
                        resources,
                        types: &mut task.types,
                        ctx: &mut worker.gen,
                        interner: &mut task.interner,
                        isa: &args.isa,
                        layouts: &mut worker.gen_layouts,
                        gen: &mut task.gen,
                    }
                    .generate(&mut task.ir_dump)
                });
            }
        });
        [generated, imported]
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

#[derive(Debug)]
pub struct CommandInfo<'a> {
    pub general: &'a str,
    pub flags: &'a [(&'a str, &'a str)],
    pub values: &'a [(&'a str, &'a str, &'a str)],
    pub inherit: Option<&'a CommandInfo<'a>>,
}

impl<'a> CommandInfo<'a> {
    fn fmt_aligned<'b, T: 'b + Copy, const N: usize>(
        f: &mut fmt::Formatter<'_>,
        elems: impl Iterator<Item = T> + Clone,
        aligned_parts: impl Fn(T) -> [&'b str; N],
        format: impl Fn(&mut fmt::Formatter<'_>, [String; N], T) -> fmt::Result,
    ) -> fmt::Result
    where
        T: Clone,
    {
        let max_lens = elems
            .clone()
            .map(&aligned_parts)
            .fold([0; N], |acc, c| acc.zip(c).map(|(a, b)| a.max(b.len())));

        for elem in elems {
            let parts = aligned_parts(elem).zip(max_lens).map(|(a, b)| {
                let mut s = a.to_string();
                s.push_str(&" ".repeat(b - a.len()));
                s
            });
            f.write_char('\t')?;
            format(f, parts, elem)?;
            f.write_char('\n')?;
        }

        Ok(())
    }

    fn iter(&self) -> impl Iterator<Item = &CommandInfo<'a>> + Clone {
        let mut current = Some(self);
        iter::from_fn(move || {
            let res = current?;
            current = current?.inherit;
            Some(res)
        })
    }
}

impl<'a> fmt::Display for CommandInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for info in self.iter() {
            f.write_str(info.general)?;
            f.write_char('\n')?;
        }

        f.write_str("FLAGS:\n")?;
        Self::fmt_aligned(
            f,
            self.iter().flat_map(|i| i.flags.iter()),
            |(n, ..)| [n],
            |f, [n], (.., d)| write!(f, "{n} - {d}"),
        )?;

        f.write_str("VALUES:\n")?;
        Self::fmt_aligned(
            f,
            self.iter().flat_map(|i| i.values.iter()),
            |(n, v, ..)| [n, v],
            |f, [n, v], (.., d)| write!(f, "{n} {v} - {d}"),
        )?;

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
    pub dump_mir: bool,
    pub dump_tir: bool,
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

            "dump-ir" => "print cranelift ir for all compiled functions"
            "dump-mir" => "print mir for all (re)compiled functions"
            "dump-tir" => "print tir for all (re)compiled functions"
        }
        values {
            "max-cores"("N") => "maximum cores used during compilation"
            "incremental-path"("string") => "where the incremental data should be stored"
            "cranelift-flags"("string") => "flags passed to cranelift, (key=value ...)"
        }
    }

    pub fn thread_count(&self) -> usize {
        thread::available_parallelism()
            .map_or(1, |n| n.get())
            .min(self.max_cores.unwrap_or(usize::MAX))
    }

    pub fn incremental_root(&self) -> &Path {
        self.incremental_path
            .as_ref()
            .and_then(|p| p.parent())
            .unwrap_or(Path::new("incremental"))
    }

    pub fn from_cli_input(
        cli_input: &CliInput,
        help: CommandInfo<'static>,
    ) -> Result<Self, MiddlewareArgsError> {
        if cli_input.enabled("help") {
            return Err(MiddlewareArgsError::Help(help));
        }

        let triple = cli_input
            .value("target")
            .map(Triple::from_str)
            .transpose()
            .map_err(MiddlewareArgsError::InvalidTargetTriple)?
            .unwrap_or_else(Triple::host);

        let flags = if let Some(flags) = cli_input.value("cranelift-flags") {
            let mut settings = settings::builder();
            let iter = flags
                .split_whitespace()
                .filter_map(|s| str::split_once(s, "="));
            for (key, value) in iter {
                settings
                    .set(key, value)
                    .map_err(|e| (key.to_owned(), value.to_owned(), e))
                    .map_err(MiddlewareArgsError::CraneliftFlag)?;
            }
            settings::Flags::new(settings)
        } else {
            settings::Flags::new(settings::builder())
        };

        let isa = Isa::new(triple, flags, false).map_err(MiddlewareArgsError::TargetIsa)?;

        Ok(MiddlewareArgs {
            path: cli_input
                .args()
                .get(1)
                .cloned()
                .unwrap_or(".".into())
                .into(),
            jit_isa: Isa::host(true).map_err(MiddlewareArgsError::HostIsa)?,
            isa,
            incremental_path: cli_input
                .value("incremental-path")
                .or(Some("incremental/default.rkyv"))
                .filter(|_| !cli_input.enabled("no-incremental"))
                .map(|s| s.into()),
            max_cores: cli_input.value("max-cores").and_then(|s| s.parse().ok()),
            dump_ir: cli_input.enabled("dump-ir"),
            dump_mir: cli_input.enabled("dump-mir"),
            dump_tir: cli_input.enabled("dump-tir"),
            check: cli_input.enabled("check"),
            quiet: cli_input.enabled("quiet"),
        })
    }

    pub fn display(&self) -> MiddlewareArgsDisplay {
        MiddlewareArgsDisplay { quiet: self.quiet }
    }
}

impl Default for MiddlewareArgs {
    fn default() -> Self {
        Self {
            path: ".".into(),
            jit_isa: Isa::host(true).unwrap(),
            isa: Isa::host(false).unwrap(),
            incremental_path: None,
            max_cores: Some(1),
            dump_ir: false,
            dump_mir: false,
            dump_tir: false,
            check: true,
            quiet: true,
        }
    }
}

compose_error! {
    MiddlewareArgsError {
        #["{inner}"]
        Help(inner: CommandInfo<'static>),
        #["invalid target triple: {inner}"]
        InvalidTargetTriple(inner: <Triple as FromStr>::Err),
        #["failed to initialize target isa: {inner}"]
        TargetIsa(inner: IsaCreationError),
        #["failed to initialize host isa: {inner}"]
        HostIsa(inner: IsaCreationError),
        #["issue with cranelift flag ({}={}): {}", inner.0, inner.1, inner.2]
        CraneliftFlag(inner: (String, String, SetError)),
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

        eprintln!("{s}");

        Ok(())
    }
}

#[derive(Debug)]
pub enum MiddlewareOutput {
    Compiled {
        binary: Vec<u8>,
        ir: Option<String>,
        mir: Option<String>,
        tir: Option<String>,
    },
    Checked {
        mir: Option<String>,
        tir: Option<String>,
    },
    Unchanged,
    Failed,
}

impl MiddlewareOutput {
    pub fn is_failed(&self) -> bool {
        matches!(self, Self::Failed)
    }

    pub fn take_binary(&mut self) -> Option<Vec<u8>> {
        match self {
            Self::Compiled { binary, .. } => Some(mem::take(binary)),
            _ => None,
        }
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
        err ref: MiddlewareLoadError,
    }
}

pub struct PackageTask {
    self_sender: SyncSender<(PackageTask, VRef<Package>)>,
    task: Task,
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

#[derive(Serialize, Deserialize, Archive)]
struct Incremental {
    resources: Resources,
    task_base: TaskBase,
    #[with(Skip)]
    worker_pool: Vec<Worker>,
    module_items: Map<VRef<Source>, ModuleItems>,
    builtin_functions: Vec<FragRef<Func>>,
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

    pub fn dump_diagnostics<'b>(
        &self,
        color: bool,
        output: &'b MiddlewareOutput,
    ) -> Result<(&'b [u8], [Option<&'b str>; 3]), String> {
        Err(match output {
            MiddlewareOutput::Compiled {
                binary,
                ir,
                mir,
                tir,
            } => return Ok((binary, [ir.as_deref(), mir.as_deref(), tir.as_deref()])),
            MiddlewareOutput::Checked { .. } => "No errors found.".into(),
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
            MiddlewareSerializerError::Inner(inner) => write!(f, "{inner}"),
            MiddlewareSerializerError::UnixTimestamp(u) => write!(f, "{u}"),
            MiddlewareSerializerError::AsString(s) => write!(f, "{s}"),
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

pub struct QuickTimer(&'static str, Instant);

static TIMERS_ENABLED: AtomicBool = AtomicBool::new(true);

impl QuickTimer {
    pub fn set_enabled(value: bool) {
        TIMERS_ENABLED.store(value, std::sync::atomic::Ordering::Relaxed)
    }

    pub fn new(message: &'static str) -> Self {
        Self(message, Instant::now())
    }

    pub fn report(self) {}
}

impl Drop for QuickTimer {
    fn drop(&mut self) {
        if !TIMERS_ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
            return;
        }
        eprintln!("timer[ {} ]: {:?}", self.0, self.1.elapsed());
    }
}
