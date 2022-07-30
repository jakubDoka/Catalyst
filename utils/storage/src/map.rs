use std::{
    collections::HashMap,
    hash::{BuildHasher, Hasher},
};

use crate::*;

/// Struct encapsulates a HashMap that is focused on storing values by [`Ident`].
/// Special hasher, that just copies the [`Ident`].index() as hash, is used. Api
/// is also a bit modified to make it nicer to work with.
#[derive(Debug, Clone)]
pub struct Map<T> {
    inner: HashMap<Ident, T, HasherFactory>,
}

impl<T> Map<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: HashMap::with_hasher(HasherFactory),
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: HashMap::with_capacity_and_hasher(capacity, HasherFactory),
        }
    }

    #[inline]
    pub fn get(&self, key: Ident) -> Option<&T> {
        self.inner.get(&key)
    }

    #[inline]
    pub fn get_mut(&mut self, key: Ident) -> Option<&mut T> {
        self.inner.get_mut(&key)
    }

    #[inline]
    pub fn insert(&mut self, key: Ident, value: T) -> Option<T> {
        self.inner.insert(key, value)
    }

    #[inline]
    pub fn insert_unique(&mut self, key: Ident, value: T) {
        assert!(self.inner.insert(key, value).is_none());
    }

    #[inline]
    pub fn remove(&mut self, key: Ident) -> Option<T> {
        self.inner.remove(&key)
    }

    #[inline]
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Ident, &T)> {
        self.inner.iter().map(|(k, v)| (*k, v))
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Ident, &mut T)> {
        self.inner.iter_mut().map(|(k, v)| (*k, v))
    }

    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = Ident> + '_ {
        self.inner.keys().copied()
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.inner.values()
    }

    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.values_mut()
    }
}

impl<T> Default for Map<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct HasherFactory;

impl BuildHasher for HasherFactory {
    type Hasher = MapHasher;

    fn build_hasher(&self) -> Self::Hasher {
        MapHasher(0)
    }
}

struct MapHasher(u32);

impl Hasher for MapHasher {
    fn finish(&self) -> u64 {
        self.0 as u64
    }

    fn write(&mut self, bytes: &[u8]) {
        debug_assert!(bytes.len() == 4);
        // SAFETY: MapHasher is only used whit complementary
        // keys with correct width.
        self.0 = unsafe { *(bytes.as_ptr() as *const u32) }
    }
}
