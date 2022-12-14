#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]
#![feature(iter_collect_into)]

mod logic;

pub use {
    diags::*,
    gen::*,
    lexing::*,
    lexing_t::*,
    logic::{
        DiagnosticView, GenTask, Incremental, MacroCtx, Middleware, MiddlewareArgs,
        MiddlewareOutput, PackageTask, Shared, Task, TaskGraph, Worker, WorkerConnections,
        WorkerState,
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
