use std::sync::mpsc::SendError;

use type_creator::type_creator;

use super::*;
use typec::*;

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
    pub(crate) state: WorkerState,
    pub(crate) context: Context,
    pub(crate) function_builder_ctx: FunctionBuilderContext,
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

    pub fn generaite_entry_point(
        &mut self,
        task: &mut Task,
        isa: &Isa,
        shared: &Shared,
        entry_points: &[CompiledFuncRef],
    ) -> CompiledFuncRef {
        self.context.func.signature = ir::Signature::new(isa.default_call_conv());
        self.context
            .func
            .signature
            .returns
            .push(ir::AbiParam::new(ir::types::I32));

        let mut generator = Generator::new(
            &mut self.state.gen_layouts,
            &mut task.gen,
            &mut self.state.gen_resources,
            &mut task.interner,
            &mut task.types,
            &task.resources.compile_requests,
            shared.resources,
        );

        let mut module = ModuleMir::default();
        let mir_func = module.dummy_func(iter::empty(), task.mir.modules.next(), Ty::U32);
        let mut builder = GenBuilder::new(
            isa,
            mir_func,
            &module,
            &mut self.context.func,
            mir_func.view(&module).types,
            &mut self.function_builder_ctx,
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

        let entry_name = generator.interner.intern_compressed(gen::ENTRY_POINT_NAME);
        let entry_func = generator.types.cache.funcs.push(Func {
            visibility: FuncVisibility::Exported,
            name: generator.interner.intern(gen::ENTRY_POINT_NAME),
            ..default()
        });
        let compiled_entry = generator.gen.get_or_insert_func(entry_name, entry_func);
        self.context.compile(&**isa).unwrap();
        generator
            .gen
            .save_compiled_code(compiled_entry, &self.context)
            .unwrap();

        compiled_entry
    }

    pub fn run<'a: 'scope, 'scope, S: AstHandler>(
        mut self,
        thread_scope: &'a thread::Scope<'scope, '_>,
        shared: Shared<'scope>,
        connections: WorkerConnections,
        source_ast_handler: &'scope mut S,
    ) -> ScopedJoinHandle<'scope, (Worker, Option<Task>)> {
        thread_scope.spawn(move || {
            let mut arena = Arena::new();
            let mut jit_ctx = JitContext::new(iter::empty());
            self.state.jit_layouts.clear(shared.jit_isa);
            self.state.gen_layouts.clear(shared.isa);
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
                        source_ast_handler,
                    );
                }
                package_task.task.modules_to_compile = modules;
                let err = connections.package_products.send((package_task, package));
                if let Err(SendError((package_task, ..))) = err {
                    return (self, Some(package_task.task));
                };
            }

            let Some(mut compile_task) = connections.gen_tasks.recv().ok() else {
                return (self, None);
            };

            let mut gen_layouts = mem::take(&mut self.state.gen_layouts);
            self.compile_current_requests(&mut compile_task, &shared, shared.isa, &mut gen_layouts);

            self.state.gen_layouts = gen_layouts;
            (self, Some(compile_task))
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
    //     //     &mut self.state.scope,
    //     //     shared.resources,
    //     //     &task.types,
    //     //     &mut task.interner,
    //     // );

    //     // loop {
    //     //     // let mut macro_ctx = mem::take(&mut self.state.macro_ctx);
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

    //     //     // self.state.macro_ctx = macro_ctx.clear();
    //     //     let mut local_macros = mem::take(&mut self.state.tir_builder_ctx.macros);
    //     //     self.compile_macros(&local_macros, task, jit_ctx, shared);
    //     //     macros.extend(local_macros.drain(..));
    //     //     self.state.tir_builder_ctx.macros = local_macros;

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
        _jit_ctx: &mut JitContext,
        shared: &Shared,
        handler: &mut S,
    ) {
        let mir_module = task.mir.modules.next();

        macro ctx() {
            BaseSourceCtx {
                worker: self,
                module,
                arena,
                task,
                shared,
                mir_module,
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

        self.state.typec_ctx.build_scope(
            module,
            shared.resources,
            &task.types,
            &mut task.interner,
            shared.builtin_functions,
        );

        loop {
            // let mut macro_ctx = mem::take(&mut self.state.macro_ctx);
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

            // self.state.macro_ctx = macro_ctx.clear();
            // let mut local_macros = mem::take(&mut self.state.typec_ctx.macros);
            // self.compile_macros(&local_macros, task, jit_ctx, shared);
            // macros.extend(local_macros.drain(..));
            // self.state.tir_builder_ctx.macros = local_macros;

            if last {
                break;
            }
        }

        if handler.save_module() {
            let module = task.mir.modules.push(self.state.module.clone());
            assert_eq!(module, mir_module);
            self.state.module.clear();

            //self.fold_constants(source, task, shared);
        }
    }

    // fn fold_constants(&mut self, _source: VRef<Source>, task: &mut Task, shared: &Shared) {
    //     for constant in self.state.just_compiled_consts.drain(..) {
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

    //         let prepared_ctx = self.state.interpreter.prepare(1 << 20, 10_000);

    //         let mut interp = Interpreter {
    //             ctx: prepared_ctx,
    //             types: &mut task.types,
    //             interner: &mut task.interner,
    //             mir: &mut task.mir,
    //             layouts: &mut self.state.jit_layouts,
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
    //             &mut self.state.jit_layouts,
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
    //    let mut layouts = mem::take(&mut self.state.jit_layouts);
    //    let compiled = self.compile_current_requests(task, shared, shared.jit_isa, &mut layouts);
    //    task.compile_requests.clear();
    //    self.state.jit_layouts = layouts;

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

    fn compile_current_requests(
        &mut self,
        task: &mut Task,
        shared: &Shared,
        isa: &Isa,
        gen_layouts: &mut GenLayouts,
    ) -> BumpVec<CompiledFuncRef> {
        let mut compiled = bumpvec![];
        for &CompileRequest {
            func,
            id,
            params,
            children,
            drops,
        } in &task.resources.compile_requests.queue
        {
            compiled.push(id);

            let Func { signature, .. } = task.types[func];
            let body = task
                .mir
                .bodies
                .get(&BodyOwner::Func(func))
                .expect("every source code function has body")
                .to_owned();
            let module = task.mir.modules.reference(body.module());
            let params = task.compile_requests.ty_slices[params].to_bumpvec();
            let view = body.view(&module);

            swap_mir_types(
                &view,
                &mut self.state.temp_dependant_types,
                &params,
                type_creator!(task),
            );

            let builder = GenBuilder::new(
                isa,
                body,
                &module,
                &mut self.context.func,
                self.state.temp_dependant_types.as_view(),
                &mut self.function_builder_ctx,
            );
            let root = body.entry();
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
            Generator::new(
                gen_layouts,
                &mut task.gen,
                &mut self.state.gen_resources,
                &mut task.interner,
                &mut task.types,
                &task.resources.compile_requests,
                shared.resources,
            )
            .generate(signature, view.ret, view.args, &params, root, builder);

            let name = task.interner.get(id.ident());
            if let Some(ref mut dump) = task.ir_dump {
                write!(dump, "{} {}", name, self.context.func.display()).unwrap();
            }

            if let err @ Err(..) = self
                .context
                .compile(&*isa.inner)
                .map_err(|err| format!("{err:?}"))
                .map(|_| ())
            {
                panic!("{:?}, {} {}", err, name, self.context.func.display());
            }
            task.gen.save_compiled_code(id, &self.context).unwrap();
            let signatures = self.context.func.dfg.signatures.values_mut();
            self.state.gen_resources.recycle_signatures(signatures);
            self.context.clear();
        }
        compiled
    }

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
    //                 if let Some(spec) = self.state.token_macros.get(&r#impl) {
    //                     let tm = jit_ctx.token_macro(spec).unwrap();
    //                     ctx.tokens.declare_macro(spec.name, tm);
    //                     continue;
    //                 }
    //             }
    //             _ => unreachable!(),
    //         }

    //         let layout =
    //             self.state
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
    //                 self.state.token_macros.insert(r#impl, r#macro);
    //             }
    //             _ => unreachable!(),
    //         }
    //     }
    // }

    fn type_check_batch(
        &mut self,
        module: VRef<Module>,
        module_ref: FragRef<ModuleMir>,
        grouped_items: GroupedItemsAst,
        arena: &Arena,
        task: &mut Task,
        shared: &Shared,
    ) {
        let mut active = Active::take(&mut self.state.typec_transfere);
        let ext = TypecExternalCtx {
            types: &mut task.types,
            interner: &mut task.interner,
            workspace: &mut task.resources.workspace,
            resources: shared.resources,
            transfere: &mut active,
            folder: &mut ConstFolderImpl {
                module,
                module_ref,
                arena,
                reused: &mut self.state.mir_ctx,
                module_ent: &mut self.state.module,
                interpreter: &mut self.state.interpreter,
                mir: &mut task.mir,
                layouts: &mut self.state.jit_layouts,
                gen: &mut task.gen,
            },
        };
        let meta = TypecMeta::new(shared.resources, module);

        TypecParser::new(arena, &mut self.state.typec_ctx, ext, meta).execute(grouped_items);

        let mut ctx = MirCompilationCtx {
            module_ent: &mut self.state.module,
            reused: &mut self.state.mir_ctx,
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

        let source = shared.resources.modules[module].source;
        Self::check_casts(
            task,
            &mut self.state.gen_layouts,
            source,
            self.state.typec_ctx.cast_checks(),
        );
        self.state.typec_transfere = active.erase();
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
    reused: &'ctx mut ReusedMirCtx,
    module_ent: &'ctx mut ModuleMir,
    interpreter: &'ctx mut InterpreterCtx,
    mir: &'ctx mut Mir,
    layouts: &'ctx mut GenLayouts,
    gen: &'ctx Gen,
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
        let meta = MirBuildMeta {
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

        let mut current = StackFrame {
            params: default(),
            block: body.entry(),
            values: default(),
            offsets: default(),
            types: default(),
            instr: 0,
            frame_base: 0,
            func: body,
        };

        current
            .types
            .extend(body.view(self.module_ent).types.values().copied());
        self.mir.modules.push(mem::take(self.module_ent));

        let prepared_ctx = self.interpreter.prepare(1 << 20, 10_000);

        let mut interp = Interpreter {
            ctx: prepared_ctx,
            types: ctx.types,
            interner: ctx.interner,
            mir: self.mir,
            layouts: self.layouts,
            current: &mut current,
            gen: self.gen,
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
            .modules
            .unextend(self.module_ref.as_slice())
            .next()
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

#[derive(Default)]
pub struct WorkerState {
    typec_ctx: TypecCtx,
    typec_transfere: TypecTransfere<'static>,
    mir_ctx: ReusedMirCtx,
    module: ModuleMir,
    // pub token_macros: Map<FragRef<Impl>, TokenMacroOwnedSpec>,
    // pub macro_ctx: MacroCtx<'static>,
    temp_dependant_types: PushMap<MirTy>,
    gen_resources: GenResources,
    jit_layouts: GenLayouts,
    gen_layouts: GenLayouts,
    interpreter: InterpreterCtx,
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

    fn should_skip_module(&mut self, ctx: BaseSourceCtx) -> bool;
    fn should_skip_manifest(&mut self, _package: VRef<Package>, _resources: &Resources) -> bool {
        true
    }

    fn parse_imports<'a>(
        &mut self,
        parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>>;

    fn imports(&mut self, header: Self::Imports<'_>, ctx: BaseSourceCtx);

    fn parse_chunk<'a>(&mut self, parser: Parser<'_, 'a, Self::Meta>) -> Option<Self::Chunk<'a>>;

    fn chunk(&mut self, items: Self::Chunk<'_>, ctx: BaseSourceCtx, macros: MacroSourceCtx)
        -> bool;

    fn save_module(&mut self) -> bool {
        false
    }
}

pub struct BaseSourceCtx<'a> {
    pub worker: &'a mut Worker,
    pub module: VRef<Module>,
    pub mir_module: FragRef<ModuleMir>,
    pub arena: &'a Arena,
    pub task: &'a mut Task,
    pub shared: &'a Shared<'a>,
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

    fn imports(&mut self, _header: Self::Imports<'_>, _ctx: BaseSourceCtx) {}

    fn chunk(
        &mut self,
        items: Self::Chunk<'_>,
        ctx: BaseSourceCtx,
        _macros: MacroSourceCtx,
    ) -> bool {
        ctx.worker.type_check_batch(
            ctx.module,
            ctx.mir_module,
            items,
            ctx.arena,
            ctx.task,
            ctx.shared,
        );
        items.last
    }

    fn parse_imports<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>> {
        parser.skip_imports()
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
