//! # Catalyst Enities
//!
//! Crate contains core datastructures used by the catalyst programming language.
//! This crate uses tons of nightly features, keep that in mind.
//!
//! ## Contents
//!
//! Main purpose of this crate is offer paralelized datastructiures with incremental
//! compilation in mind. Things are highly specialized for processing acyclic dependency
//! graphs, but also offers thread local specialized allocators for performant temporary
//! allocations.
//!
//! Some datastructures need to be persistent, and so they implement
//! [rkyv](https://crates.io/crates/rkyv) traits.
//!
//! Package also reexports some crates for convenience of catalyst compiler.
//!
//! More information in respective item docs.

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
    slice_iter_mut_as_mut_slice,
    once_cell
)]

mod allocator;
mod bump_vec;
mod frag_map;
mod map;
mod pool;
mod primitives;

pub use {
    crate::bump_vec::{BumpAlloc, BumpAllocRef, BumpVec, ToBumpVec, BUMP_ALLOC},
    allocator::{
        arena::{Arena, ProxyArena},
        code::{Align, Code, CodeAllocator, CodeGuard, CodeRelocator},
        Allocator, ALLOCATOR_POOL,
    },
    arc_swap, dashmap,
    frag_map::{
        addr::{FragAddr, FragSliceAddr, NonMaxError, NonMaxU16, NonMaxU32, NonMaxU64, NonMaxU8},
        interner::{ident::Ident, Interner, InternerBase},
        relocator::{
            DynFragMap, FragMarks, FragRelocMarker, FragRelocator, Relocated, RelocatedObjects,
        },
        shadow::{ShadowFragBase, ShadowFragMap},
        sync::{FragSliceKey, SyncFragBase, SyncFragBorrow, SyncFragMap, SyncFragView},
        Cluster, ClusterBorrow, FragBase, FragMap, NoInteriorMutability,
    },
    map::{CMap, CSet, FvnBuildHasher, Map, Set},
    pool::{Pool, Pooled},
    primitives::{
        CtlOption, FragRef, FragRefSlice, FragSlice, NoShortCircuitCollect, OptFragRef, OptVRef,
        TransposeOption, VRef, VRefSlice, VSlice,
    },
    smallvec,
};
