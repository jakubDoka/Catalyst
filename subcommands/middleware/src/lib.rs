#![feature(let_else)]
#![feature(const_trait_impl)]
#![feature(let_chains)]
#![feature(slice_group_by)]
#![feature(default_free_fn)]

mod cmd_args;
mod incremental;
mod scheduler;
mod state_gen;

pub use {
    incremental::{Incremental, IncrementalBorrow, InternerTransfer, SweepCtx},
    scheduler::{Scheduler, Task, Worker},
};
