#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(negative_impls)]
#![feature(let_else)]
#![feature(inline_const)]
#![feature(let_chains)]

mod allocator;
mod arena;
mod bump_alloc;

pub use {
    crate::bump_alloc::{BumpVec, ToBumpVec},
    allocator::Allocator,
    arena::Arena,
};
