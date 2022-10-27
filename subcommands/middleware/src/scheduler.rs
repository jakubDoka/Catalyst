use std::{
    iter, mem,
    path::*,
    sync::mpsc::{self, Receiver, Sender},
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

#[derive(Default)]
pub struct Scheduler {
    pub interner: Interner,
    pub workspace: Workspace,
    pub resources: Resources,
    pub package_graph: PackageGraph,
    pub resource_loading_ctx: PackageLoaderCtx,
}

impl Scheduler {
    pub fn new() -> Self {
        Self::default()
    }

    fn reload_resources(&mut self, path: &Path) {
        package_loader!(self).reload(path, &mut self.resource_loading_ctx);
    }

    pub fn update(&mut self, args: SchedulerArgs) {
        self.reload_resources(args.path);

        let (sender, receiver) = mpsc::channel();
        // num cores form std
        let num_workers = thread::available_parallelism().map_or(1, |n| n.get());
        let mut workers = iter::repeat_with(|| Worker::new(sender.clone()))
            .take(num_workers)
            .collect::<Vec<_>>();

        thread::scope(|s| {});
    }
}

pub struct Shared<'a> {
    resources: &'a Resources,
    jit_isa: &'a Isa,
}

pub struct SchedulerArgs<'a> {
    pub path: &'a Path,
    pub isa: &'a Isa,
}

pub struct Worker {
    pub tasks: Receiver<Task>,
    pub products: Sender<Task>,
    pub state: WorkerState,
    pub context: Context,
    pub function_builder_ctx: FunctionBuilderContext,
}

impl Worker {
    pub fn new(products: Sender<Task>) -> (Self, Sender<Task>) {
        let (tx, rx) = mpsc::channel();
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
            loop {
                let Ok(mut task) = self.tasks.recv() else {
                    break;
                };

                let mut modules = mem::take(&mut task.modules_to_compile);
                for module in modules.drain(..) {
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

pub struct Task {
    pub modules_to_compile: Vec<VRef<Module>>,
    pub interner: Interner,
    pub workspace: Workspace,
    pub typec: Typec,
    pub mir: Mir,
    pub gen: Gen,
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
