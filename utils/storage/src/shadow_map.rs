use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use crate::VPtr;

pub struct ShadowMap<K, T> {
    data: Vec<T>,
    default: T,
    _ph: PhantomData<fn(K) -> K>,
}

impl<K, T: Default> ShadowMap<K, T> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<K: VPtr, T> Index<K> for ShadowMap<K, T> {
    type Output = T;

    fn index(&self, key: K) -> &T {
        self.data.get(key.index()).unwrap_or(&self.default)
    }
}

impl<K: VPtr, T: Default + Clone> IndexMut<K> for ShadowMap<K, T> {
    fn index_mut(&mut self, key: K) -> &mut T {
        let index = key.index();
        self.data
            .resize(self.data.len().max(index + 1), Default::default());
        &mut self.data[index]
    }
}

impl<K, T: Default> Default for ShadowMap<K, T> {
    fn default() -> Self {
        ShadowMap {
            data: Vec::new(),
            default: T::default(),
            _ph: PhantomData,
        }
    }
}
