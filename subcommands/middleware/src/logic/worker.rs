use std::sync::mpsc::SendError;

use super::*;

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

    pub fn run<'a: 'scope, 'scope, S: SourceAstHandler>(
        mut self,
        thread_scope: &'a thread::Scope<'scope, '_>,
        shared: Shared<'scope>,
        connections: WorkerConnections,
        source_ast_handler: &'scope mut S,
    ) -> ScopedJoinHandle<'scope, (Worker, Option<Task>)> {
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

    //     // let mut macros = typec::build_scope(
    //     //     module,
    //     //     &mut self.state.scope,
    //     //     shared.resources,
    //     //     &task.typec,
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

    fn compile_module<S: SourceAstHandler>(
        &mut self,
        module: VRef<Module>,
        arena: &mut Arena,
        task: &mut Task,
        jit_ctx: &mut JitContext,
        shared: &Shared,
        handler: &mut S,
    ) {
        macro ctx() {
            BaseSourceCtx {
                worker: self,
                module,
                arena,
                task,
                shared,
            }
        }

        let source = shared.resources.modules[module].source;

        let content = &shared.resources.sources[source].content;
        let mut parser_ctx = ParserCtx::new(content);

        macro parser() {
            Parser::new(
                &mut task.interner,
                &mut task.workspace,
                &mut parser_ctx,
                arena,
                source,
                content,
            )
        }

        let Some(result) = handler.parse_imports(parser!()) else {return};
        handler.imports(result, ctx!());

        let mut macros = typec::build_scope(
            module,
            &mut self.state.scope,
            shared.resources,
            &task.typec,
            &mut task.interner,
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
        if macros.is_empty() || task.workspace.has_errors() {
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

            if task.gen[id].inner.is_some() {
                continue;
            }

            let Func { signature, .. } = task.typec[func];

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

            swap_mir_types(
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
                let name = &task.interner[task.typec[func].name];
                write!(dump, "{} {}", name, self.context.func.display()).unwrap();
                // print!("{} {}", name, self.context.func.display());
            }
            self.context.compile(&*isa.inner).unwrap();
            task.gen.save_compiled_code(id, &self.context).unwrap();
            let signatures = self.context.func.dfg.signatures.values_mut();
            self.state.gen_resources.recycle_signatures(signatures);
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
            let Impl { methods, .. } = task.typec[r#impl];

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
                let id = task.gen.get_or_insert(key, || CompiledFunc::new(func, key));
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
            let frontier = task.typec[methods]
                .iter()
                .map(collector)
                .collect::<BumpVec<_>>();

            Task::traverse_compile_requests(frontier, slice::from_mut(task), shared.jit_isa)
        };

        macros.iter().flat_map(extractor).collect()
    }

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
    //         let impl_ent = task.typec.impls[r#impl];
    //         let spec = impl_ent.key.spec.base(&task.typec);

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
    //                 .ty_layout(ty, &[], &mut task.typec, &mut task.interner);
    //         let params = task.typec[params].to_bumpvec();
    //         let funcs = task.typec.func_slices[impl_ent.methods]
    //             .iter()
    //             .map(|&func| {
    //                 Generator::func_instance_name(
    //                     true,
    //                     &isa.triple,
    //                     func,
    //                     params.iter().copied(),
    //                     &task.typec,
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
            .filter(|&func| task.typec[func].flags.contains(FuncFlags::ENTRY))
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

    fn check_casts(
        typec: &mut Typec,
        interner: &mut Interner,
        layouts: &mut GenLayouts,
        origin: VRef<Source>,
        workspace: &mut Workspace,
        checks: &mut Vec<CastCheck>,
    ) {
        for CastCheck {
            loc: span,
            from,
            to,
        } in checks.drain(..)
        {
            let from_param_presence = typec.contains_params_low(from);
            let to_param_presence = typec.contains_params_low(to);
            match from_param_presence.combine(to_param_presence) {
                ParamPresence::Present => {
                    workspace.push(CastBetweenGenericTypes {
                        from: typec.display_ty(from, interner),
                        to: typec.display_ty(to, interner),
                        loc: SourceLoc { origin, span },
                    });
                    continue;
                }
                ParamPresence::BehindPointer => continue,
                ParamPresence::Absent => (),
            }

            let from_layout = layouts.ty_layout(from, &[], typec, interner);
            let to_layout = layouts.ty_layout(to, &[], typec, interner);
            if from_layout.size != to_layout.size {
                workspace.push(CastSizeMismatch {
                    from: typec.display_ty(from, interner),
                    from_size: from_layout.size,
                    to: typec.display_ty(to, interner),
                    to_size: to_layout.size,
                    loc: SourceLoc { origin, span },
                });
            } else if from_layout.align < to_layout.align {
                workspace.push(CastAlignMismatch {
                    from: typec.display_ty(from, interner),
                    from_align: from_layout.align,
                    to: typec.display_ty(to, interner),
                    to_align: to_layout.align,
                    loc: SourceLoc { origin, span },
                });
            }
        }
    }
}

#[derive(Default)]
pub struct WorkerState {
    scope: Scope,
    ty_checker_ctx: TyCheckerCtx,
    ast_transfer: AstTransfer<'static>,
    mir_ctx: MirCtx,
    mir_move_ctx: MirMoveCtx,
    // pub token_macros: Map<FragRef<Impl>, TokenMacroOwnedSpec>,
    // pub macro_ctx: MacroCtx<'static>,
    tir_builder_ctx: TirBuilderCtx,
    temp_dependant_types: FuncTypes,
    gen_resources: GenResources,
    jit_layouts: GenLayouts,
    gen_layouts: GenLayouts,
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

pub trait SourceAstHandler: Sync + Send {
    type Meta: TokenMeta;
    type Imports<'a>;
    type Chunk<'a>;

    fn parse_imports<'a>(
        &mut self,
        parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>>;

    fn imports(&mut self, header: Self::Imports<'_>, ctx: BaseSourceCtx);

    fn parse_chunk<'a>(&mut self, parser: Parser<'_, 'a, Self::Meta>) -> Option<Self::Chunk<'a>>;

    fn chunk(&mut self, items: Self::Chunk<'_>, ctx: BaseSourceCtx, macros: MacroSourceCtx)
        -> bool;
}

pub struct BaseSourceCtx<'a> {
    worker: &'a mut Worker,
    module: VRef<Module>,
    arena: &'a Arena,
    task: &'a mut Task,
    shared: &'a Shared<'a>,
}

pub struct MacroSourceCtx;

#[derive(Clone)]
pub struct DefaultSourceAstHandler;

impl SourceAstHandler for DefaultSourceAstHandler {
    type Meta = NoTokenMeta;

    type Imports<'a> = ();

    type Chunk<'a> = GroupedItemsAst<'a>;

    fn imports(&mut self, _header: Self::Imports<'_>, _ctx: BaseSourceCtx) {}

    fn chunk(
        &mut self,
        items: Self::Chunk<'_>,
        ctx: BaseSourceCtx,
        _macros: MacroSourceCtx,
    ) -> bool {
        ctx.worker
            .type_check_batch(ctx.module, items, ctx.arena, ctx.task, ctx.shared);
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
}
