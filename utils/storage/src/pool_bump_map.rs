use std::ops::{Deref, DerefMut};

use crate::{BumpMap, VPtr, Maybe, PoolMap, Invalid, Frames};

pub struct PoolBumpMap<K, T, C, CACHE> {
    inner: BumpMap<K, T, C, CACHE>,
    heads: Vec<Maybe<Private>>,
    freed: PoolMap<Private, (K, Maybe<Private>)>,
    tmp: Vec<T>,
}

impl<K, T, C, CACHE: Default> PoolBumpMap<K, T, C, CACHE> {
    pub fn new() -> Self {
        Self {
            inner: BumpMap::new(),
            heads: Vec::new(),
            freed: PoolMap::new(),
            tmp: Vec::new(),
        }
    }
}

impl<K: VPtr, T, C, CACHE> PoolBumpMap<K, T, C, CACHE> {
    pub fn bump_slice(&mut self, slice: &[T]) -> Maybe<K> 
        where
            T: Clone
    {
        self.bump(slice.iter().cloned())
    }

    pub fn bump(&mut self, items: impl IntoIterator<Item = T>) -> Maybe<K> {
        self.tmp.extend(items);
        self.bump_prepared()
    }

    fn bump_prepared(&mut self) -> Maybe<K> {
        if let Some(Some(head)) = self.heads.get(self.tmp.len()).map(|v| v.expand()) {
            let (id, next) = self.freed.remove(head);
            self.heads[self.tmp.len()] = next;
            self.inner
                .get_mut(id)
                .iter_mut()
                .zip(self.tmp.drain(..))
                .for_each(|(a, b)| *a = b);
            Maybe::some(id)
        } else {
            self.inner.bump(self.tmp.drain(..))
        }
    }
}

impl<K, T, C: VPtr, CACHE> PoolBumpMap<K, T, C, CACHE> {
    pub fn bump_pushed(&mut self) -> Maybe<K>
        where
            K: VPtr
    {
        self.tmp.extend(self.inner.discard_pushed());
        self.bump_prepared()
    }
}

impl<K: VPtr, T, C> PoolBumpMap<K, T, C, Frames<T>> {
    pub fn bump_cached(&mut self) -> Maybe<K> {
        self.tmp.extend(self.inner.discard_cache());
        self.bump_prepared()
    }
}

impl<K, T, C, CACHE: Default> Default for PoolBumpMap<K, T, C, CACHE> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, T, C, CACHE> Deref for PoolBumpMap<K, T, C, CACHE> {
    type Target = BumpMap<K, T, C, CACHE>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<K, T, C, CACHE> DerefMut for PoolBumpMap<K, T, C, CACHE> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

gen_v_ptr!(Private);