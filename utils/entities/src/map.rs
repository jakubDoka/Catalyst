use std::{
    collections::{HashMap, HashSet},
    default::default,
    hash::*,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{FragMarks, Relocated};
use dashmap::{DashMap, DashSet};

pub type Map<K, V> = HashMap<K, V, FvnBuildHasher>;
pub type Set<T> = HashSet<T, FvnBuildHasher>;
pub type CSet<K> = Arc<DashSet<K, FvnBuildHasher>>;

/// Wrapper around DashMap that implements `rkyv` traits.
#[repr(transparent)]

pub struct CMap<K, V> {
    inner: DashMap<K, V, FvnBuildHasher>,
}

impl<K: Relocated + Eq + Hash, V: Relocated> Relocated for CMap<K, V> {
    fn mark(&self, marker: &mut crate::FragRelocMarker) {
        self.inner.mark(marker);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        self.inner.remap(ctx)
    }
}

impl<K: Eq + Hash, V> Default for CMap<K, V> {
    fn default() -> Self {
        Self { inner: default() }
    }
}

impl<K, V> Deref for CMap<K, V> {
    type Target = DashMap<K, V, FvnBuildHasher>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<K, V> DerefMut for CMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<K: Hash + Eq, V> FromIterator<(K, V)> for CMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self {
            inner: iter.into_iter().collect(),
        }
    }
}

const FVN_PRIME: u64 = 0x00000100000001B3;
const FVN_OFFSET: u64 = 0xcbf29ce484222325;

pub type FvnBuildHasher = BuildHasherDefault<FvnHasher>;

/// Simply because pulling a crate for this is overkill.
pub struct FvnHasher(u64);

impl Default for FvnHasher {
    fn default() -> Self {
        Self(FVN_OFFSET)
    }
}

impl Hasher for FvnHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.0 = self.0.wrapping_mul(FVN_PRIME);
            self.0 ^= byte as u64;
        }
    }
}
