use std::{
    iter, mem,
    path::*,
    sync::mpsc::{self, Receiver, Sender},
    thread,
};

use diags::*;
use mir::*;
use mir_t::*;
use packaging::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;
use typec::*;
use typec_t::*;

use crate::*;

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

    pub fn reload_resources(&mut self, path: &Path) {
        package_loader!(self).reload(path, &mut self.resource_loading_ctx);
    }

    pub fn update(&mut self, path: &Path) {
        self.reload_resources(path);

        let (sender, receiver) = mpsc::channel();
        // num cores form std
        let num_workers = thread::available_parallelism().map_or(1, |n| n.get());
        let mut workers = iter::repeat_with(|| Worker::new(sender.clone()))
            .take(num_workers)
            .collect::<Vec<_>>();

        thread::scope(|s| {})
    }
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
            loop {
                let Ok(mut task) = self.tasks.recv() else {
                    break;
                };

                let mut modules = mem::take(&mut task.modules_to_compile);
                for module in modules.drain(..) {
                    self.compile_module(module, &mut arena, &mut task, &shared);
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
        shared: &Shared,
    ) {
        let source = shared.resources.modules[module].source;

        self.parse::<UseAstSkip>(source, arena, task, shared);

        loop {
            let Some(grouped_items) = self.parse::<GroupedItemsAst>(source, arena, task, shared) else {
                continue;
            };

            self.type_check_batch(module, grouped_items, arena, task, shared);
            let last = grouped_items.last;
            arena.clear();

            if last {
                break;
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
        shared: &Shared,
    ) -> Option<T>
    where
        T::Args: Default,
    {
        ParsingCtx::new(
            shared.resources.sources[source].content.as_str(),
            &mut self.state.parsing_state,
            arena,
            &mut task.workspace,
            &mut task.interner,
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
}

#[derive(Default)]
pub struct WorkerState {
    pub parsing_state: ParsingState,
    pub scope: Scope,
    pub ty_checker_ctx: TyCheckerCtx,
    pub ast_transfer: AstTransfer<'static>,
    pub mir_builder_ctx: MirBuilderCtx,
}
