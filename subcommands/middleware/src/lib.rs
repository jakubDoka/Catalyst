#![feature(let_else)]
#![feature(const_trait_impl)]
#![feature(let_chains)]

mod cmd_args;
mod incremental;
mod scheduler;
mod state_gen;

pub use {
    incremental::{Incremental, SweepCtx},
    scheduler::{Scheduler, Task, Worker},
};
