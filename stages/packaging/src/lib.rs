//! Crate implements project loader and simple package manager via git.

#![feature(let_else)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(never_type)]

mod packages;
mod scheduler;
mod state_gen;

pub use {scheduler::Scheduler, state_gen::PackageLoader};
