use std::{collections::HashMap, hash::*};

use crate::*;

pub trait SpecialHash: Eq + Hash + Copy {
    type BuildHasher: BuildHasher + Default;
}

pub type Map<K, V> = HashMap<K, V, <K as SpecialHash>::BuildHasher>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdentPair(pub VRef<str>, pub VRef<str>);

impl SpecialHash for IdentPair {
    type BuildHasher = PairHasherFactory;
}

impl SpecialHash for VRef<str> {
    type BuildHasher = SimpleHasherFactory;
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for IdentPair {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            std::mem::transmute::<_, u64>(*self).hash(state);
        }
    }
}

impl Invalid for IdentPair {
    unsafe fn invalid() -> Self {
        IdentPair(VRef::invalid(), VRef::invalid())
    }
    fn is_invalid(&self) -> bool {
        self.0.is_invalid()
    }
}

#[derive(Debug, Clone)]
pub struct SimpleHasherFactory;

impl BuildHasher for SimpleHasherFactory {
    type Hasher = SimpleMapHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        SimpleMapHasher(0)
    }
}

impl Default for SimpleHasherFactory {
    #[inline]
    fn default() -> Self {
        Self
    }
}

pub struct SimpleMapHasher(u32);

impl Hasher for SimpleMapHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0 as u64
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        debug_assert!(bytes.len() == 4);
        // SAFETY: MapHasher is only used whit complementary
        // keys with correct width.
        self.0 = unsafe { *(bytes.as_ptr() as *const u32) }
    }
}

#[derive(Debug, Clone)]
pub struct PairHasherFactory;

impl BuildHasher for PairHasherFactory {
    type Hasher = PairMapHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        PairMapHasher(0)
    }
}

impl Default for PairHasherFactory {
    #[inline]
    fn default() -> Self {
        Self
    }
}

pub struct PairMapHasher(u64);

impl Hasher for PairMapHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        debug_assert!(bytes.len() == 8);
        // SAFETY: MapHasher is only used whit complementary
        // keys with correct width.
        self.0 = unsafe { *(bytes.as_ptr() as *const u64) }
    }
}
