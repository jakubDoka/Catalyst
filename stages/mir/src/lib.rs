#![feature(let_else)]
#![feature(try_blocks)]
#![feature(iter_intersperse)]

mod builder;
mod display;
mod state_gen;

pub use state_gen::MirChecker;
