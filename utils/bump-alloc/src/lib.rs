#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(negative_impls)]
#![feature(inline_const)]
#![feature(let_chains)]
#![feature(slice_from_ptr_range)]
#![feature(rustc_attrs)]
#![feature(const_trait_impl)]
#![feature(const_discriminant)]
#![feature(atomic_mut_ptr)]
#![feature(never_type)]

mod alloc_tree;
mod allocator;
mod arena;
mod bump_vec;
mod frag_map;
mod primitives;

pub use {
    crate::bump_vec::{BumpVec, ToBumpVec},
    allocator::{Allocator, AllocatorLow, ProtectedAllocator},
    arena::Arena,
    frag_map::{FragAddr, FragBase, FragMap, FragSliceAddr, NonMaxU16, MAX_FRAGMENT_SIZE},
    primitives::{
        CtlOption, FragRef, FragRefSlice, FragSlice, NoShortCircuitCollect, OptFragRef, OptVRef,
        TransposeOption, VRef, VRefDefault, VRefSlice, VSlice,
    },
};
