#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]
#![feature(iter_collect_into)]
#![feature(decl_macro)]
#![feature(trivial_bounds)]
#![feature(closure_lifetime_binder)]

mod logic;

pub use {
    cli,
    diags::*,
    gen::*,
    lexing::*,
    span::*,
    logic::{
        task::{Task, TaskGraph},
        worker::{
            AstHandler, BaseSourceCtx, MacroSourceCtx, Worker, WorkerConnections, WorkerState,
        },
        CommandInfo, DiagnosticView, GenTask, Incremental, Middleware, MiddlewareArgs,
        MiddlewareArgsError, MiddlewareOutput, PackageTask, QuickTimer, Shared,
    },
    borrowc::*,
    mir_t::*,
    packaging::*,
    resources::*,
    parsing::*,
    ast::*,
    snippet_display::*,
    storage::*,
    types::*,
    types::*,
};
