#![feature(try_blocks)]
#![feature(iter_intersperse)]
#![feature(slice_group_by)]
#![feature(if_let_guard)]
#![feature(default_free_fn)]
#![feature(never_type)]
#![feature(let_chains)]

mod builder;
mod display;
mod patterns;
mod state_gen;

pub use {
    patterns::{Branch, Node, PatNode, PatNodeChildren, PatTree, Range, UpperBound},
    state_gen::MirChecker,
};
