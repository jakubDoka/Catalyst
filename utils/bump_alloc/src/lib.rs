#![feature(allocator_api)]

mod bump_alloc;

pub use crate::bump_alloc::{BumpVec, ToBumpVec};
