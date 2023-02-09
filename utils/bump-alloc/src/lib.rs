#![allow(incomplete_features)]
#![feature(
    allocator_api,
    new_uninit,
    negative_impls,
    inline_const,
    let_chains,
    slice_from_ptr_range,
    rustc_attrs,
    const_trait_impl,
    const_discriminant,
    atomic_mut_ptr,
    never_type,
    default_free_fn,
    iter_collect_into,
    pointer_is_aligned,
    slice_index_methods,
    slice_group_by,
    ptr_metadata,
    auto_traits,
    if_let_guard,
    int_roundings,
    const_option,
    const_slice_index,
    specialization,
    slice_iter_mut_as_mut_slice
)]

mod alloc_tree;
mod allocator;
mod arena;
mod bump_vec;
mod frag_map;
mod map;
mod primitives;

pub use {
    crate::bump_vec::{BumpAlloc, BumpAllocRef, BumpVec, ToBumpVec, BUMP_ALLOC},
    allocator::{Allocator, AllocatorFrame, AllocatorLow, ProtectedAllocator},
    arc_swap,
    arena::Arena,
    dashmap,
    frag_map::{
        addr::{FragAddr, FragSliceAddr, NonMaxError, NonMaxU16, NonMaxU32, NonMaxU64},
        interner::{ident::Ident, Interner, InternerBase},
        relocator::{
            DashMapFilterUnmarkedKeys, DynFragMap, FragMarks, FragRelocMarker, FragRelocator,
            IsMarked, Relocated, RelocatedObjects,
        },
        sync::{FragSliceKey, SyncFragBase, SyncFragMap, SyncFragView},
        ArcSwapArchiver, DashMapArchiver, FragBase, FragMap, NoInteriorMutability,
        SmallVecArchiver,
    },
    map::{CMap, CSet, FvnBuildHasher, Map, Set},
    primitives::{
        CtlOption, FragRef, FragRefSlice, FragSlice, NoShortCircuitCollect, OptFragRef, OptVRef,
        TransposeOption, VRef, VRefDefault, VRefSlice, VSlice,
    },
    smallvec,
};
