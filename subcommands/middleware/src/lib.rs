#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]
#![feature(iter_collect_into)]

mod cmd_args;
mod logic;

pub use logic::{
    GenTask, Incremental, MacroCtx, Middleware, MiddlewareArgs, MiddlewareOutput, PackageTask,
    Shared, Task, TaskGraph, Worker, WorkerConnections, WorkerState,
};
