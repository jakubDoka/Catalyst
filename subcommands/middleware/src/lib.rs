#![feature(let_else)]
#![feature(const_trait_impl)]

mod cmd_args;
mod incremental;
mod scheduler;
mod state_gen;

pub use scheduler::{Scheduler, Task, Worker};
