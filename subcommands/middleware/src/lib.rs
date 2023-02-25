#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]
#![feature(iter_collect_into)]
#![feature(decl_macro)]
#![feature(trivial_bounds)]
#![feature(closure_lifetime_binder)]
#![feature(array_zip)]
#![feature(is_some_and)]

mod logic;

pub use {
    ast::*,
    borrowc::*,
    cli,
    diags::*,
    gen::*,
    lexing::*,
    logic::{
        task::{Task, TaskGraph},
        worker::{
            AstHandler, BaseSourceCtx, MacroSourceCtx, Worker, WorkerConnections, WorkerState,
        },
        CommandInfo, DiagnosticView, GenTask, Incremental, Middleware, MiddlewareArgs,
        MiddlewareArgsError, MiddlewareOutput, PackageTask, QuickTimer, Shared,
    },
    mir::*,
    packaging::*,
    parsing::*,
    resources::*,
    snippet_display::*,
    span::*,
    storage::*,
    types::*,
    types::*,
};
