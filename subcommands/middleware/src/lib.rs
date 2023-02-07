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
    lexing_t::*,
    logic::{
        task::{Task, TaskGraph},
        worker::{
            AstHandler, BaseSourceCtx, MacroSourceCtx, Worker, WorkerConnections, WorkerState,
        },
        CommandInfo, DiagnosticView, GenTask, Incremental, Middleware, MiddlewareArgs,
        MiddlewareArgsError, MiddlewareOutput, PackageTask, QuickTimer, Shared,
    },
    mir::*,
    mir_t::*,
    packaging::*,
    packaging_t::*,
    parsing::*,
    parsing_t::*,
    snippet_display::*,
    storage::*,
    typec::*,
    typec_t::*,
};
