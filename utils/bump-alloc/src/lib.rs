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
#![feature(default_free_fn)]
#![feature(iter_collect_into)]

pub extern crate dashmap;

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
        FragAddr, FragBase, FragMap, FragRelocator, FragSliceAddr, NonMaxU16, MAX_FRAGMENT_SIZE,
    },
    map::{CMap, Map, Set},
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

    use dashmap::DashMap;

    pub type Map<K, V> = HashMap<K, V, FvnBuildHasher>;
    pub type CMap<K, V> = Arc<DashMap<K, V, FvnBuildHasher>>;
    pub type Set<T> = HashSet<T, FvnBuildHasher>;

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
