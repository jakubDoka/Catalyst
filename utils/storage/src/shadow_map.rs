use std::{
    default::default,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use crate::VRef;

pub struct ShadowMap<T, V> {
    data: Vec<V>,
    default: V,
    phantom: PhantomData<fn(T) -> T>,
}

impl<T, V: Default> ShadowMap<T, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self)
    where
        V: Clone,
    {
        self.data.fill(self.default.clone());
    }
}

impl<T, V> Index<VRef<T>> for ShadowMap<T, V> {
    type Output = V;

    fn index(&self, key: VRef<T>) -> &V {
        self.data.get(key.index()).unwrap_or(&self.default)
    }
}

impl<T, V: Default + Clone> IndexMut<VRef<T>> for ShadowMap<T, V> {
    fn index_mut(&mut self, key: VRef<T>) -> &mut V {
        let index = key.index();
        self.data.resize(self.data.len().max(index + 1), default());
        &mut self.data[index]
    }
}

impl<T, V: Default> Default for ShadowMap<T, V> {
    fn default() -> Self {
        ShadowMap {
            data: Vec::new(),
            default: V::default(),
            phantom: PhantomData,
        }
    }
}
