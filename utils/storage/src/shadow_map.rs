use std::{
    default::default,
    marker::PhantomData,
    mem,
    ops::{Index, IndexMut},
};

use rkyv::{Archive, Deserialize, Serialize};

use crate::VRef;

#[derive(Debug, Archive, Serialize, Deserialize)]
pub struct ShadowMap<T, V> {
    data: Vec<V>,
    default: V,
    phantom: PhantomData<fn(T) -> T>,
}

impl<T, V: Default> ShadowMap<T, V> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T, V: Default> ShadowMap<T, V> {
    pub fn clear(&mut self)
    where
        V: Clone,
    {
        self.data.fill(self.default.clone());
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<T>, &V)> {
        self.data.iter().enumerate().map(|(i, v)| (VRef::new(i), v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (VRef<T>, &mut V)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (VRef::new(i), v))
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut()
    }

    pub fn get_array<const SIZE: usize>(&mut self, keys: [VRef<T>; SIZE]) -> [&mut V; SIZE]
    where
        V: Clone,
    {
        // assert no duplicates, array is expected to be small
        assert!({
            keys.iter()
                .all(|key| keys.iter().filter(|&k| k == key).count() == 1)
        });

        // SAFETY: we have no duplicate indexes
        keys.map(|key| unsafe { mem::transmute(&mut self[key]) })
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

impl<T, V: Clone> Clone for ShadowMap<T, V> {
    fn clone(&self) -> Self {
        ShadowMap {
            data: self.data.clone(),
            default: self.default.clone(),
            phantom: PhantomData,
        }
    }
}
