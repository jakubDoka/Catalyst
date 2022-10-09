#![feature(let_else)]
#![feature(try_blocks)]
#![feature(iter_intersperse)]
#![feature(slice_group_by)]

mod builder;
mod display;
mod patterns;
mod state_gen;

pub use {
    patterns::{
        Branch, Node, PatternError, PatternNode, PatternNodeChildren, PatternSolver, PatternTree,
        Range, UpperBound,
    },
    state_gen::MirChecker,
};
