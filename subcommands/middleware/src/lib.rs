#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]
#![feature(iter_collect_into)]
#![feature(decl_macro)]

mod logic;

pub use {
    diags::*,
    gen::*,
    lexing::*,
    lexing_t::*,
    logic::{
        task::{Task, TaskGraph},
        worker::{SourceAstHandler, Worker, WorkerConnections, WorkerState},
        DiagnosticView, GenTask, Incremental, Middleware, MiddlewareArgs, MiddlewareOutput,
        PackageTask, Shared,
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
