use std::{
    iter, mem,
    path::*,
    sync::mpsc::{self, Receiver, Sender},
    thread,
};

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

        thread::scope(|s| {})
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
}

impl Worker {
    pub fn new(products: Sender<Task>) -> (Self, Sender<Task>) {
        let (tx, rx) = mpsc::channel();
        (
            Self {
                tasks: rx,
                products,
                state: WorkerState::default(),
            },
            tx,
        )
    }

    pub fn run<'a: 'b, 'b>(mut self, thread_scope: &'a thread::Scope<'b, '_>, shared: Shared<'b>) {
        thread_scope.spawn(move || {
            let mut arena = Arena::new();
            let mut jit_ctx = JitContext::new(iter::empty());
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

        let mut macro_ctx = mem::take(&mut self.state.macro_ctx);
        let token_macros = typec::build_scope(
            module,
            &mut self.state.scope,
            shared.resources,
            &task.typec,
            &mut task.interner,
        );
        self.load_macros(&mut macro_ctx, token_macros, task, jit_ctx);

        loop {
            let Some(grouped_items) = self.parse::<GroupedItemsAst>(source, arena, task, Some(&macro_ctx.tokens), shared) else {
                continue;
            };

            let macro_checkpoint = task.typec.module_items[module].macros.len();
            self.type_check_batch(module, grouped_items, arena, task, shared);
            let last = grouped_items.last;
            arena.clear();

            let new_macros =
                task.typec.module_items[module].macros[macro_checkpoint..].to_bumpvec();
            if !new_macros.is_empty() {}

            if last {
                break;
            }
        }

        self.state.macro_ctx = macro_ctx.clear();
    }

    fn load_macros<'macros>(
        &mut self,
        ctx: &mut MacroCtx<'macros>,
        token_macros: BumpVec<VRef<Impl>>,
        task: &mut Task,
        jit_ctx: &'macros JitContext,
    ) {
        for macro_impl in token_macros {
            let r#impl = task.typec.impls[macro_impl];
            let spec = r#impl.key.spec.base(&task.typec);

            match spec {
                s if s == SpecBase::TOKEN_MACRO => {
                    if let Some(spec) = self.state.token_macros.get(macro_impl) {
                        let tm = jit_ctx.token_macro(spec).expect("Well then...");
                        ctx.tokens.declare_macro(spec.name, tm);
                        continue;
                    }
                }
                _ => unreachable!(),
            }

            let layout = self.state.jit_layouts.ty_layout(
                r#impl.key.ty,
                &[],
                &mut task.typec,
                &mut task.interner,
            );
            let name = task.interner.intern_with(|s, t| {
                snake_case(
                    &s[match r#impl.key.ty {
                        Ty::Struct(s) => task.typec[s].name,
                        Ty::Enum(e) => task.typec[e].name,
                        _ => todo!("Make a macro attribute for this that will supply name."),
                    }],
                    t,
                )
            });
            let funcs = task.typec.func_slices[r#impl.methods]
                .iter()
                .map(|&func| task.typec.funcs[func].name)
                .filter_map(|name| task.gen.compiled_funcs.index(name));

            match spec {
                s if s == SpecBase::TOKEN_MACRO => {
                    let r#macro = TokenMacroOwnedSpec::new(layout.rust_layout(), name, funcs)
                        .expect("Deng it!");
                    let tm = jit_ctx.token_macro(&r#macro).expect("That sucks...");
                    ctx.tokens.declare_macro(r#macro.name, tm);
                    self.state.token_macros.insert(macro_impl, r#macro);
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
    pub jit_layouts: GenLayouts,
    pub ast_transfer: AstTransfer<'static>,
    pub mir_builder_ctx: MirBuilderCtx,
    pub token_macros: SparseMap<Impl, TokenMacroOwnedSpec>,
    pub macro_ctx: MacroCtx<'static>,
    pub tir_builder_ctx: TirBuilderCtx,
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

fn snake_case(input: &str, output: &mut String) -> Option<()> {
    let mut chars = input.chars();
    output.push(chars.next()?.to_ascii_lowercase());

    for c in chars {
        if c.is_uppercase() {
            output.push('_');
        }
        output.push(c.to_ascii_lowercase());
    }

    Some(())
}
