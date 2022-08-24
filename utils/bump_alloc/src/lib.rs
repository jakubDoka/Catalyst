#![feature(allocator_api)]

mod bump_alloc;

pub use bump_alloc::{BumpVec, ToBumpVec};
