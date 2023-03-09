use type_creator::type_creator;

use super::*;
use typec::*;

pub(super) struct WorkerConnections {
    pub(super) package_tasks: Receiver<(PackageTask, VRef<Package>)>,
    pub(super) package_products: Sender<(PackageTask, VRef<Package>)>,
}

type ConnsReturn = (WorkerConnections, SyncSender<(PackageTask, VRef<Package>)>);

impl WorkerConnections {
    pub fn new(package_products: Sender<(PackageTask, VRef<Package>)>) -> ConnsReturn {
        let (package_dump, package_tasks) = mpsc::sync_channel(0);
        (
            Self {
                package_tasks,
                package_products,
            },
            package_dump,
        )
    }
}

#[derive(Default)]
pub(super) struct Worker {
    pub(super) typec_ctx: TypecCtx,
    pub(super) typec_transfer: TypecTransfer<'static>,
    pub(super) mir_ctx: BorrowcCtx,
    pub(super) module: ModuleMir,
    // pub token_macros: Map<FragRef<Impl>, TokenMacroOwnedSpec>,
    // pub macro_ctx: MacroCtx<'static>,
    pub(super) jit_layouts: GenLayouts,
    pub(super) gen_layouts: GenLayouts,
    pub(super) interpreter: InterpreterCtx,
    pub(super) gen: GeneratorThreadCtx,
    requests: CompileRequests,
    request_ctx: CompileRequestCollectorCtx,
}

impl Worker {
    pub(super) fn generaite_entry_point(
        &mut self,
        task: &mut Task,
        isa: &Isa,
        shared: &Shared,
        entry_points: &[CompiledFuncRef],
    ) -> CompiledFuncRef {
        self.gen.cranelift.func.signature = ir::Signature::new(isa.default_call_conv());
        self.gen
            .cranelift
            .func
            .signature
            .returns
            .push(ir::AbiParam::new(ir::types::I32));

        let mut generator = Generator {
            layouts: &mut self.gen_layouts,
            gen: &mut task.gen,
            gen_resources: &mut self.gen.resources,
            interner: &mut task.interner,
            types: &mut task.types,
            request: CompileRequestView::default(),
            resources: shared.resources,
        };

        let mut module = ModuleMir::default();
        let mir_func = module.dummy_func(iter::empty(), task.mir.next_module(), Ty::U32);
        let mut builder = GenBuilder::new(
            isa,
            mir_func,
            &module,
            &mut self.gen.cranelift.func,
            mir_func.view(&module).types,
            &mut self.gen.function_builder,
        );

        let entry = builder.create_block();
        builder.switch_to_block(entry);

        let mut exit_code = None;
        for &func in entry_points {
            let (func_ref, ..) = generator.import_compiled_func(func, iter::empty(), &mut builder);
            let inst = builder.ins().call(func_ref, &[]);
            exit_code = builder.inst_results(inst).first().copied();
        }

        let exit_code = if let Some(exit_code) = exit_code {
            match builder
                .func
                .dfg
                .value_type(exit_code)
                .bytes()
                .cmp(&ir::types::I32.bytes())
            {
                std::cmp::Ordering::Less => builder.ins().uextend(ir::types::I32, exit_code),
                std::cmp::Ordering::Equal => exit_code,
                std::cmp::Ordering::Greater => builder.ins().ireduce(ir::types::I32, exit_code),
            }
        } else {
            builder.ins().iconst(ir::types::I32, 0)
        };
        builder.ins().return_(&[exit_code]);

        builder.seal_block(entry);
        builder.finalize();

        let entry_name = generator.interner.intern_compressed(gen::ENTRY_POINT_NAME);
        let entry_func = generator.types.cache.funcs.push(Func {
            visibility: FuncVisibility::Exported,
            name: generator.interner.intern(gen::ENTRY_POINT_NAME),
            ..default()
        });
        let (compiled_entry, ..) = generator.gen.get_or_insert_func(entry_name, entry_func);
        self.gen.cranelift.compile(&**isa).unwrap();
        generator
            .gen
            .save_compiled_code(entry_func, compiled_entry, &self.gen.cranelift)
            .unwrap();
        self.gen.cranelift.clear();

        compiled_entry
    }

    pub(super) fn run<'a: 'scope, 'scope, S: AstHandler>(
        mut self,
        thread_scope: &'a thread::Scope<'scope, '_>,
        shared: Shared<'scope>,
        connections: WorkerConnections,
        ast_handler: &'scope mut S,
    ) -> ScopedJoinHandle<'scope, Worker> {
        thread_scope.spawn(move || {
            let mut arena = Arena::default();
            let mut jit_ctx = JitContext::new(iter::empty());

            self.jit_layouts.clear(shared.jit_isa);
            self.gen_layouts.clear(shared.isa);

            while let Ok((mut package_task, package)) = connections.package_tasks.recv() {
                let modules = mem::take(&mut package_task.task.modules_to_compile);
                for &module in modules.iter() {
                    self.compile_module(
                        module,
                        &mut arena,
                        &mut package_task.task,
                        &mut jit_ctx,
                        &shared,
                        ast_handler,
                    );
                }
                package_task.task.modules_to_compile = modules;

                if connections
                    .package_products
                    .send((package_task, package))
                    .is_err()
                {
                    break; // in case of ast walker this is desired behavior
                }
            }

            self
        })
    }

    // fn compile_module(
    //     &mut self,
    //     module: VRef<Module>,
    //     arena: &mut Arena,
    //     task: &mut Task,
    //     jit_ctx: &mut JitContext,
    //     shared: &Shared,
    // ) {
    //     todo!()

    //     // let source = shared.resources.modules[module].source;

    //     // let mut parser_ctx = ParserCtx::new(&shared.resources.sources[source].content);
    //     // self.parser(
    //     //     source,
    //     //     arena,
    //     //     task,
    //     //     &mut parser_ctx,
    //     //     shared,
    //     //     Parser::skip_imports,
    //     // );

    //     // let mut macros = types::build_scope(
    //     //     module,

    //     //     shared.resources,
    //     //     &task.types,
    //     //     &mut task.interner,
    //     // );

    //     // loop {

    //     //     // self.load_macros(
    //     //     //     &mut macro_ctx,
    //     //     //     macros.iter().copied(),
    //     //     //     task,
    //     //     //     jit_ctx,
    //     //     //     shared.jit_isa,
    //     //     // );
    //     //     let Some(grouped_items) = self.parser(source, arena, task, &mut parser_ctx, shared, Parser::grouped_items) else {
    //     //         continue;
    //     //     };

    //     //     self.type_check_batch(module, grouped_items, arena, task, shared);
    //     //     let last = grouped_items.last;
    //     //     arena.clear();

    //     //     self.compile_macros(&local_macros, task, jit_ctx, shared);
    //     //     macros.extend(local_macros.drain(..));

    //     //     if last {
    //     //         break;
    //     //     }
    //     // }
    // }

    fn compile_module<S: AstHandler>(
        &mut self,
        module: VRef<Module>,
        arena: &mut Arena,
        task: &mut Task,
        jit_ctx: &mut JitContext,
        shared: &Shared,
        handler: &mut S,
    ) {
        let mir_module = task.mir.next_module();

        macro ctx() {
            BaseSourceCtx {
                worker: self,
                module,
                arena,
                task,
                shared,
                mir_module,
                jit: jit_ctx,
            }
        }

        if handler.should_skip_module(ctx!()) {
            return;
        }

        let source = shared.resources.modules[module].source;

        let content = &shared.resources.sources[source].content;

        let mut parser_ctx = ParserCtx::new(content);

        macro parser() {
            Parser::new(
                &mut task.interner,
                &mut task.resources.workspace,
                &mut parser_ctx,
                arena,
                source,
                content,
            )
        }

        let Some(result) = handler.parse_imports(parser!()) else {return};
        handler.imports(result, ctx!());

        self.typec_ctx.build_scope(
            module,
            shared.resources,
            &task.types,
            &mut task.interner,
            shared.builtin_functions,
        );

        loop {
            // self.load_macros(
            //     &mut macro_ctx,
            //     macros.iter().copied(),
            //     task,
            //     jit_ctx,
            //     shared.jit_isa,
            // );
            let Some(chunk) = handler.parse_chunk(parser!()) else {
                continue;
            };
            let last = handler.chunk(chunk, ctx!(), MacroSourceCtx);

            arena.clear();

            // self.compile_macros(&local_macros, task, jit_ctx, shared);
            // macros.extend(local_macros.drain(..));

            if last {
                break;
            }
        }

        if handler.save_module() {
            task.mir.insert_module(mir_module, self.module.clone());
            self.module.clear();
        }
    }

    // fn fold_constants(&mut self, _source: VRef<Source>, task: &mut Task, shared: &Shared) {

    //         let body = task
    //             .mir
    //             .bodies
    //             .get(&BodyOwner::Const(constant))
    //             .unwrap()
    //             .to_owned();

    //         let module = &task.mir.modules[body.module()];

    //         let mut current = StackFrame {
    //             params: default(),
    //             block: body.entry(),
    //             values: default(),
    //             offsets: default(),
    //             types: default(),
    //             instr: 0,
    //             frame_base: 0,
    //             func: body,
    //         };

    //         current
    //             .types
    //             .extend(body.view(module).types.values().copied());

    //         let mut interp = Interpreter {
    //             ctx: prepared_ctx,
    //             types: &mut task.types,
    //             interner: &mut task.interner,
    //             mir: &mut task.mir,

    //             current: &mut current,
    //             gen: &task.gen,
    //         };

    //         let value = match interp.interpret() {
    //             Ok(v) => v,
    //             Err(err) => {
    //                 let snip = TodoSnippet {
    //                     message: format!("Failed to fold constant: {:?}", err),
    //                     loc: task.types[constant]
    //                         .loc
    //                         .source_loc(&task.types, shared.resources),
    //                 };
    //                 task.workspace.push(snip);
    //                 continue;
    //             }
    //         };

    //         task.gen.save_const(
    //             constant,
    //             value,

    //             &mut task.types,
    //             &mut task.interner,
    //         );
    //     }
    // }

    //fn compile_macros(
    //    &mut self,
    //    macros: &[MacroCompileRequest],
    //    task: &mut Task,
    //    jit_ctx: &mut JitContext,
    //    shared: &Shared,
    //) {
    //    if macros.is_empty() || task.workspace.has_errors() {
    //        return;
    //    }

    //    let imported = self.push_macro_compile_requests(macros, task, shared);

    //    let compiled = self.compile_current_requests(task, shared, shared.jit_isa, &mut layouts);
    //    task.compile_requests.clear();

    //    jit_ctx
    //        .load_functions(
    //            compiled.into_iter().chain(imported),
    //            &task.gen,
    //            &task.types,
    //            &task.interner,
    //            false,
    //        )
    //        .unwrap();
    //    jit_ctx.prepare_for_execution();
    //}

    //fn push_macro_compile_requests(
    //    &mut self,
    //    macros: &[MacroCompileRequest],
    //    task: &mut Task,
    //    shared: &Shared,
    //) -> BumpVec<CompiledFuncRef> {
    //    let extractor = |&MacroCompileRequest { r#impl, params, .. }| {
    //        let Impl { methods, .. } = task.types[r#impl];

    //        let params = task.types[params].to_bumpvec();
    //        // todo try to avoid moving and allocate ty VSlice right away
    //        let pushed_params = task.compile_requests.ty_slices.bump_slice(&params);

    //        let collector = |&func| {
    //            let key = Generator::func_instance_name(
    //                true,
    //                &shared.jit_isa.triple,
    //                func,
    //                params.iter().copied(),
    //                &task.types,
    //                &mut task.interner,
    //            );
    //            let id = task.gen.get_or_insert_func(key, func);
    //            (
    //                CompileRequestChild {
    //                    func,
    //                    id,
    //                    params: pushed_params,
    //                },
    //                // we only have one thread
    //                Ok(0),
    //            )
    //        };
    //        let frontier = task.types[methods]
    //            .iter()
    //            .map(collector)
    //            .collect::<BumpVec<_>>();

    //        Task::traverse_compile_requests(frontier, slice::from_mut(task), shared.jit_isa)
    //    };

    //    macros.iter().flat_map(extractor).collect()
    //}

    // fn load_macros<'macros>(
    //     &mut self,
    //     ctx: &mut MacroCtx<'macros>,
    //     macros: impl IntoIterator<Item = MacroCompileRequest>,
    //     task: &mut Task,
    //     jit_ctx: &'macros JitContext,
    //     isa: &Isa,
    // ) {
    //     if task.workspace.has_errors() {
    //         return;
    //     }

    //     for MacroCompileRequest {
    //         name,
    //         r#impl,
    //         ty,
    //         params,
    //         ..
    //     } in macros
    //     {
    //         let impl_ent = task.types.impls[r#impl];
    //         let spec = impl_ent.key.spec.base(&task.types);

    //         match spec {
    //             s if s == SpecBase::TOKEN_MACRO => {

    //                     let tm = jit_ctx.token_macro(spec).unwrap();
    //                     ctx.tokens.declare_macro(spec.name, tm);
    //                     continue;
    //                 }
    //             }
    //             _ => unreachable!(),
    //         }

    //         let layout =

    //                 .jit_layouts
    //                 .ty_layout(ty, &[], &mut task.types, &mut task.interner);
    //         let params = task.types[params].to_bumpvec();
    //         let funcs = task.types.func_slices[impl_ent.methods]
    //             .iter()
    //             .map(|&func| {
    //                 Generator::func_instance_name(
    //                     true,
    //                     &isa.triple,
    //                     func,
    //                     params.iter().copied(),
    //                     &task.types,
    //                     &mut task.interner,
    //                 )
    //             })
    //             .filter_map(|key| task.gen.get(key));

    //         match spec {
    //             s if s == SpecBase::TOKEN_MACRO => {
    //                 let r#macro = TokenMacroOwnedSpec::new(layout.rust_layout(), name, funcs)
    //                     .expect("all functions should be present");
    //                 let tm = jit_ctx
    //                     .token_macro(&r#macro)
    //                     .expect("all functions should be present");
    //                 ctx.tokens.declare_macro(r#macro.name, tm);

    //             }
    //             _ => unreachable!(),
    //         }
    //     }
    // }

    fn type_check_chunk<'a>(
        &mut self,
        module: VRef<Module>,
        module_ref: FragRef<ModuleMir>,
        grouped_items: GroupedItemsAst<'a>,
        arena: &'a Arena,
        task: &mut Task,
        shared: &Shared,
        jit_ctx: &mut JitContext,
    ) -> Active<TypecTransfer<'a>> {
        let mut active = Active::take(&mut self.typec_transfer);
        let ext = TypecExternalCtx {
            types: &mut task.types,
            interner: &mut task.interner,
            workspace: &mut task.resources.workspace,
            resources: shared.resources,
            transfer: &mut active,
            folder: &mut ConstFolderImpl {
                module,
                module_ref,
                arena,
                reused: &mut self.mir_ctx,
                module_ent: &mut self.module,
                interpreter: &mut self.interpreter,
                mir: &mut task.mir,
                layouts: &mut self.jit_layouts,
                gen: &mut task.gen,
                jitter: JitterImpl {
                    requests: &mut self.requests,
                    isa: shared.jit_isa,
                    resources: shared.resources,
                    request_ctx: &mut self.request_ctx,
                    thread_ctx: &mut self.gen,
                },
                jit: jit_ctx,
            },
        };
        let meta = TypecMeta::new(shared.resources, module);

        TypecParser::new(arena, &mut self.typec_ctx, ext, meta).execute(grouped_items);
        active
    }

    fn verify_chunk(
        &mut self,
        module: VRef<Module>,
        module_ref: FragRef<ModuleMir>,
        grouped_items: GroupedItemsAst,
        arena: &Arena,
        task: &mut Task,
        shared: &Shared,
        jit_ctx: &mut JitContext,
    ) {
        let active = self.type_check_chunk(
            module,
            module_ref,
            grouped_items,
            arena,
            task,
            shared,
            jit_ctx,
        );

        let mut ctx = MirCompilationCtx {
            module_ent: &mut self.module,
            reused: &mut self.mir_ctx,
            mir: &mut task.mir,
            types: &mut task.types,
            interner: &mut task.interner,
            workspace: &mut task.resources.workspace,
            arena,
            resources: shared.resources,
        };

        borrowc::compile_functions(module, module_ref, active.checked_funcs(), &mut ctx);

        active
            .checked_funcs()
            .iter()
            .filter(|&&(func, ..)| task.types[func].flags.contains(FuncFlags::ENTRY))
            .map(|(func, ..)| func)
            .collect_into(&mut task.resources.entry_points);

        if let Some(ref mut tir_dump) = task.tir_dump {
            TirDisplay {
                interner: &mut task.interner,
                types: &mut task.types,
                resources: shared.resources,
            }
            .display_funcs(active.checked_funcs(), tir_dump)
            .unwrap();
        }

        if let Some(ref mut mir_dump) = task.mir_dump {
            MirDisplay {
                module: &self.module,
                interner: &task.interner,
                types: &task.types,
                resources: shared.resources,
                mir: &task.mir,
            }
            .display_funcs(active.checked_funcs().iter().map(|&(f, ..)| f), mir_dump)
            .unwrap();
        }

        let source = shared.resources.modules[module].source;
        Self::check_casts(
            task,
            &mut self.gen_layouts,
            source,
            self.typec_ctx.cast_checks(),
        );
        self.typec_transfer = active.erase();
    }

    fn check_casts(
        task: &mut Task,
        layouts: &mut GenLayouts,
        origin: VRef<Source>,
        checks: impl Iterator<Item = CastCheck>,
    ) {
        for CastCheck { loc, from, to } in checks {
            let loc = SourceLoc { origin, span: loc };
            let from_param_presence = task.types.contains_params_low(from);
            let to_param_presence = task.types.contains_params_low(to);
            match from_param_presence.combine(to_param_presence) {
                ParamPresence::Present => {
                    CastBetweenGenericTypes {
                        from: type_creator!(task).display(from),
                        to: type_creator!(task).display(to),
                        loc,
                    }
                    .add(&mut task.workspace);
                    continue;
                }
                ParamPresence::BehindPointer => continue,
                ParamPresence::Absent => (),
            }

            let from_layout = layouts.ty_layout(from, &[], type_creator!(task));
            let to_layout = layouts.ty_layout(to, &[], type_creator!(task));
            if from_layout.size != to_layout.size {
                CastSizeMismatch {
                    from: type_creator!(task).display(from),
                    from_size: from_layout.size,
                    to: type_creator!(task).display(to),
                    to_size: to_layout.size,
                    loc,
                }
                .add(&mut task.workspace);
            } else if from_layout.align < to_layout.align {
                CastAlignMismatch {
                    from: type_creator!(task).display(from),
                    from_align: from_layout.align,
                    to: type_creator!(task).display(to),
                    to_align: to_layout.align,
                    loc,
                }
                .add(&mut task.workspace);
            }
        }
    }
}

struct ConstFolderImpl<'arena, 'ctx> {
    module: VRef<Module>,
    module_ref: FragRef<ModuleMir>,
    arena: &'arena Arena,
    reused: &'ctx mut BorrowcCtx,
    module_ent: &'ctx mut ModuleMir,
    interpreter: &'ctx mut InterpreterCtx,
    mir: &'ctx mut Mir,
    layouts: &'ctx mut GenLayouts,
    gen: &'ctx mut Gen,
    jitter: JitterImpl<'ctx>,
    jit: &'ctx mut JitContext,
}

impl<'arena, 'ctx> ConstFolderImpl<'arena, 'ctx> {
    fn fold(&mut self, ret: Ty, tir_body: TirNode, ctx: ConstFolderContext) -> Option<IValue> {
        let ext = ExternalMirCtx {
            types: ctx.types,
            interner: ctx.interner,
            workspace: ctx.workspace,
            arena: self.arena,
            resources: ctx.resources,
        };
        let meta = BorrowcMeta {
            source: ctx.resources.modules[self.module].source,
            module: self.module,
            no_moves: false,
        };
        let body = MirBuilder::new(
            ret,
            &[],
            ext,
            meta,
            self.module_ent,
            self.module_ref,
            self.reused,
        )
        .build(default(), tir_body)?;

        let mut current = StackFrame::new(body, body.view(self.module_ent).types.values().copied());

        self.mir
            .insert_module(self.module_ref, mem::take(self.module_ent));

        let prepared_ctx = self.interpreter.prepare(1 << 20, 10_000);

        let mut interp = Interpreter {
            ctx: prepared_ctx,
            types: ctx.types,
            interner: ctx.interner,
            mir: self.mir,
            layouts: self.layouts,
            current: &mut current,
            gen: self.gen,
            jitter: &mut self.jitter,
            jit: &mut self.jit,
        };

        let value = match interp.interpret() {
            Ok(v) => v,
            Err(err) => {
                let snip = TodoSnippet {
                    message: format!("Failed to fold constant: {err:?}"),
                    loc: meta.source_loc(tir_body.span),
                };
                ctx.workspace.push(snip)?;
            }
        };

        *self.module_ent = self
            .mir
            .remove_module(self.module_ref)
            .expect("module was pushed previously");

        self.module_ent.roll_back(body);

        value
    }
}

impl ConstFolder for ConstFolderImpl<'_, '_> {
    fn fold(&mut self, ty: Ty, body: TirNode, ctx: ConstFolderContext) -> FolderValue {
        let Some(value) = self.fold(ty, body, ctx) else {
            return default();
        };

        match value {
            IValue::Register(val) => FolderValue::new_register(val as u64),
            IValue::Memory(_) => todo!(),
        }
    }
}

struct JitterImpl<'ctx> {
    requests: &'ctx mut CompileRequests,
    isa: &'ctx Isa,
    resources: &'ctx Resources,
    request_ctx: &'ctx mut CompileRequestCollectorCtx,
    thread_ctx: &'ctx mut GeneratorThreadCtx,
}

impl Jitter for JitterImpl<'_> {
    fn jit(&mut self, ctx: &mut JitterCtx) -> Result<(), JitterError> {
        let (generated, imported) = CompileRequestCollector {
            requests: self.requests,
            isa: self.isa,
            types: ctx.types,
            interner: ctx.interner,
            gen: ctx.gen,
            ctx: self.request_ctx,
            mir: ctx.mir,
        }
        .distribute_compile_requests(iter::once(ctx.func));

        GeneratorThread {
            requests: self.requests.as_shard(),
            mir: ctx.mir,
            resources: self.resources,
            types: ctx.types,
            ctx: self.thread_ctx,
            interner: ctx.interner,
            isa: self.isa,
            layouts: ctx.layouts,
            gen: ctx.gen,
        }
        .generate(&mut None);

        ctx.jit
            .load_functions(
                generated.into_iter().chain(imported),
                ctx.gen,
                ctx.types,
                ctx.interner,
            )
            // .map_err(|err| {
            //     if let JitRelocError::MissingSymbol(GenItemName::Func(sym)) = err {
            //         dbg!(ctx.interner.get(sym.ident()));
            //     }
            // })
            .unwrap();

        Ok(())
    }
}

// #[derive(Default)]
// pub struct MacroCtx<'macros> {
//     pub tokens: TokenMacroCtx<'macros>,
// }

// impl<'macros> MacroCtx<'macros> {
//     pub fn clear<'detached>(self) -> MacroCtx<'detached> {
//         MacroCtx {
//             tokens: self.tokens.clear(),
//         }
//     }
// }

pub trait AstHandler: Sync + Send {
    type Meta: TokenMeta;
    type Imports<'a>;
    type Chunk<'a>;

    fn parse_chunk<'a>(&mut self, parser: Parser<'_, 'a, Self::Meta>) -> Option<Self::Chunk<'a>>;
    fn chunk(&mut self, items: Self::Chunk<'_>, ctx: BaseSourceCtx, macros: MacroSourceCtx)
        -> bool;

    fn parse_manifest<'a>(
        &mut self,
        _parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<ManifestAst<'a, Self::Meta>> {
        None
    }

    fn manifest(
        &mut self,
        _manifest: ManifestAst<Self::Meta>,
        _source: VRef<Source>,
        _resources: &Resources,
    ) {
    }

    fn should_skip_module(&mut self, ctx: BaseSourceCtx) -> bool {
        ctx.shared.resources.is_external(ctx.module)
    }

    fn should_skip_manifest(&mut self, _package: VRef<Package>, _resources: &Resources) -> bool {
        true
    }

    fn parse_imports<'a>(
        &mut self,
        parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>>;

    fn imports(&mut self, _header: Self::Imports<'_>, _ctx: BaseSourceCtx) {}

    fn save_module(&mut self) -> bool {
        false
    }
}

pub struct BaseSourceCtx<'a> {
    worker: &'a mut Worker,
    pub module: VRef<Module>,
    pub mir_module: FragRef<ModuleMir>,
    pub arena: &'a Arena,
    pub task: &'a mut Task,
    pub shared: &'a Shared<'a>,
    pub jit: &'a mut JitContext,
}

pub struct MacroSourceCtx;

#[derive(Clone)]
pub struct DefaultSourceAstHandler;

impl AstHandler for DefaultSourceAstHandler {
    type Meta = NoTokenMeta;

    type Imports<'a> = ();

    type Chunk<'a> = GroupedItemsAst<'a>;

    fn should_skip_module(&mut self, _ctx: BaseSourceCtx) -> bool {
        false
    }

    fn parse_imports<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>> {
        parser.skip_imports()
    }

    fn imports(&mut self, _header: Self::Imports<'_>, _ctx: BaseSourceCtx) {}

    fn chunk(
        &mut self,
        items: Self::Chunk<'_>,
        ctx: BaseSourceCtx,
        _macros: MacroSourceCtx,
    ) -> bool {
        ctx.worker.verify_chunk(
            ctx.module,
            ctx.mir_module,
            items,
            ctx.arena,
            ctx.task,
            ctx.shared,
            ctx.jit,
        );
        items.last
    }

    fn parse_chunk<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Chunk<'a>> {
        parser.grouped_items()
    }

    fn save_module(&mut self) -> bool {
        true
    }
}

pub(super) struct GeneratorThread<'a> {
    pub(super) requests: CompileRequestsShard<'a>,
    pub(super) mir: &'a Mir,
    pub(super) resources: &'a Resources,
    pub(super) types: &'a mut Types,
    pub(super) ctx: &'a mut GeneratorThreadCtx,
    pub(super) interner: &'a mut Interner,
    pub(super) isa: &'a Isa,
    pub(super) layouts: &'a mut GenLayouts,
    pub(super) gen: &'a mut Gen,
}

impl<'a> GeneratorThread<'a> {
    pub(super) fn generate(&mut self, ir_dump: &mut Option<String>) {
        for req @ &CompileRequest {
            func, id, params, ..
        } in self.requests.queue
        {
            let Func { signature, .. } = self.types[func];
            let body = self
                .mir
                .get_func(func, &self.types.cache.funcs)
                .expect("every source code function has body")
                .to_owned();
            let module = self.mir.reference_module(body.module());
            let params = &self.requests.ty_slices[params];
            let view = body.view(&module);

            swap_mir_types(&view, &mut self.ctx.temp_types, params, type_creator!(self));

            let builder = GenBuilder::new(
                self.isa,
                body,
                &module,
                &mut self.ctx.cranelift.func,
                self.ctx.temp_types.as_view(),
                &mut self.ctx.function_builder,
            );
            let root = body.entry();

            Generator {
                layouts: self.layouts,
                gen: self.gen,
                gen_resources: &mut self.ctx.resources,
                interner: self.interner,
                types: self.types,
                request: req.view(&self.requests),
                resources: self.resources,
            }
            .generate(signature, view.ret, view.args, params, root, builder);

            if let Some(ref mut dump) = ir_dump {
                let name = self.interner.get(id.ident());
                write!(dump, "{} {}", name, self.ctx.cranelift.func.display()).unwrap();
            }

            if let err @ Err(..) = self
                .ctx
                .cranelift
                .compile(&*self.isa.inner)
                .map_err(|err| format!("{err:?}"))
                .map(|_| ())
            {
                let name = self.interner.get(id.ident());
                panic!("{:?}, {} {}", err, name, self.ctx.cranelift.func.display());
            }

            self.gen
                .save_compiled_code(func, id, &self.ctx.cranelift)
                .unwrap();
            self.ctx.clear();
        }
    }
}

pub struct GeneratorThreadCtx {
    pub temp_types: PushMap<TyMir>,
    pub cranelift: Context,
    pub function_builder: FunctionBuilderContext,
    pub resources: GenResources,
}

impl Default for GeneratorThreadCtx {
    fn default() -> Self {
        Self {
            temp_types: PushMap::new(),
            cranelift: Context::new(),
            function_builder: FunctionBuilderContext::new(),
            resources: GenResources::default(),
        }
    }
}

impl GeneratorThreadCtx {
    fn clear(&mut self) {
        let signatures = self.cranelift.func.dfg.signatures.values_mut();
        self.resources.recycle_signatures(signatures);
        self.cranelift.clear();
    }
}
