#![feature(let_else)]
#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]

mod cmd_args;
mod incremental;
mod logic;

pub use {
    incremental::{Incremental, IncrementalBorrow, InternerTransfer, SweepCtx},
    logic::{
        GenTask, MacroCtx, Middleware, MiddlewareArgs, Shared, Task, TaskGraph, Worker, WorkerState,
    },
};
