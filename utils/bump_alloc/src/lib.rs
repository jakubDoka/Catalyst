#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(negative_impls)]
#![feature(let_else)]
#![feature(inline_const)]
#![feature(let_chains)]
#![feature(slice_from_ptr_range)]

mod alloc_tree;
mod allocator;
mod arena;
mod bump_vec;

pub use {
    crate::bump_vec::{BumpVec, ToBumpVec},
    allocator::{Allocator, ProtectedAllocator},
    arena::Arena,
};
