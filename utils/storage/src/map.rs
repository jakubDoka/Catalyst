use std::{collections::HashMap, hash::*, sync::Arc};

use dashmap::DashMap;

pub type Map<K, V> = HashMap<K, V, FvnBuildHasher>;
pub type CMap<K, V> = Arc<DashMap<K, V, FvnBuildHasher>>;

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
