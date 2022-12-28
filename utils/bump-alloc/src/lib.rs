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
    slice_group_by
)]

pub extern crate dashmap;
pub extern crate smallvec;

mod alloc_tree;
mod allocator;
mod arena;
mod bump_vec;
mod frag_map;
mod primitives;

pub use {
    crate::bump_vec::{BumpAlloc, BumpAllocRef, BumpVec, ToBumpVec, BUMP_ALLOC},
    allocator::{Allocator, AllocatorLow, ProtectedAllocator},
    arena::Arena,
    frag_map::{
        addr::{FragAddr, FragSliceAddr, NonMaxU32, NonMaxU64},
        relocator::{
            DynFragMap, FragMarks, FragRelocMapping, FragRelocMarker, FragRelocator, Relocated,
            RelocatedObjects,
        },
        FragBase, FragMap,
    },
    map::{CMap, CSet, Map, Set},
    primitives::{
        CtlOption, FragRef, FragRefSlice, FragSlice, NoShortCircuitCollect, OptFragRef, OptVRef,
        TransposeOption, VRef, VRefDefault, VRefSlice, VSlice,
    },
};

mod map {
    use std::{
        collections::{HashMap, HashSet},
        hash::*,
        sync::Arc,
    };

    use dashmap::{DashMap, DashSet};

    pub type Map<K, V> = HashMap<K, V, FvnBuildHasher>;
    pub type CMap<K, V> = DashMap<K, V, FvnBuildHasher>;
    pub type Set<T> = HashSet<T, FvnBuildHasher>;
    pub type CSet<K> = Arc<DashSet<K, FvnBuildHasher>>;

    const FVN_PRIME: u64 = 0x00000100000001B3;
    const FVN_OFFSET: u64 = 0xcbf29ce484222325;

    #[derive(Default, Clone, Copy)]
    pub struct FvnBuildHasher;

    impl BuildHasher for FvnBuildHasher {
        type Hasher = FvnHasher;

        fn build_hasher(&self) -> Self::Hasher {
            FvnHasher(FVN_OFFSET)
        }
    }

    pub struct FvnHasher(u64);

    impl Hasher for FvnHasher {
        fn finish(&self) -> u64 {
            self.0
        }

        fn write(&mut self, bytes: &[u8]) {
            for &byte in bytes {
                self.0 = self.0.wrapping_mul(FVN_PRIME);
                self.0 ^= byte as u64;
            }
        }
    }
}
