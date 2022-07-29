use std::{collections::HashMap, hash::{BuildHasher, Hasher}, marker::PhantomData};

use crate::*;

#[derive(Debug, Clone)]
pub struct Map<K, T> {
    inner: HashMap<Ident, T, HasherFactory>,
    _ph: PhantomData<fn(K) -> K>
}



impl<K, T> Map<K, T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: HashMap::with_hasher(HasherFactory),
            _ph: PhantomData
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: HashMap::with_capacity_and_hasher(capacity, HasherFactory),
            _ph: PhantomData
        }
    }
}

impl<K: VPtr, T> Map<K, T> {
    
    #[inline]
    pub fn get(&self, key: MapRef<K>) -> Option<&T> {
        self.inner.get(&key.as_ident())
    }

    #[inline]
    pub fn get_mut(&mut self, key: MapRef<K>) -> Option<&mut T> {
        self.inner.get_mut(&key.as_ident())
    }

    #[inline]
    pub fn insert(&mut self, key: Ident, value: T) -> (MapRef<K>, Option<T>) {
        (MapRef(K::new(key.index())), self.inner.insert(key, value))
    }

    #[inline]
    pub fn insert_unique(&mut self, key: Ident, value: T) -> MapRef<K> {
        let (k, shadow) = self.insert(key, value);
        assert!(shadow.is_none());
        k
    }

    #[inline]
    pub fn remove(&mut self, key: MapRef<K>) -> Option<T> {
        self.inner.remove(&key.as_ident())
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
    pub fn iter(&self) -> impl Iterator<Item = (&Ident, &T)> {
        self.inner.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&Ident, &mut T)> {
        self.inner.iter_mut()
    }

    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &Ident> {
        self.inner.keys()
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

impl<K, T> Default for Map<K, T> {
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

pub struct MapRef<T>(T);

impl<T: VPtr> MapRef<T> {
    pub fn as_ident(&self) -> Ident {
        Ident::new(self.0.index())
    }
}