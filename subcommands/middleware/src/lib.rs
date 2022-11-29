#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]

mod cmd_args;
mod logic;

pub use logic::{
    GenTask, MacroCtx, Middleware, MiddlewareArgs, MiddlewareOutput, Shared, Task, TaskGraph,
    Worker, WorkerState,
};
