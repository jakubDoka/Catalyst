use std::{mem::replace, ops::{Index, IndexMut}};

use crate::*;

pub struct OrderedMap<K, V, C> {
    index: ShadowMap<K, Maybe<C>>,
    data: PoolMap<C, (K, V)>,
}

impl<K, V, C: Invalid> OrderedMap<K, V, C> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<K: VPtr, V, C: VPtr> OrderedMap<K, V, C> {
    pub fn insert(&mut self, key: K, value: V) -> (C, Option<V>) {
        let index = self.data.push((key, value));
        let shadow = replace(&mut self.index[key], Maybe::some(index));
        (index, shadow.expand().map(|shadow| self.data.remove(shadow).1))
    }

    pub fn remove(&mut self, key: K) -> Option<V> {
        let index = self.index[key].expand()?;
        let (found_key, value) = self.data.remove(index);
        assert!(found_key == key);
        self.index[key] = Maybe::none();
        Some(value)
    }

    pub fn remove_index(&mut self, index: C) -> (K, V) {
        let (key, value) = self.data.remove(index);
        self.index[key] = Maybe::none();
        (key, value)
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.index[key].expand().map(|index| &self.data[index].1)
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.index[key].expand().map(|index| &mut self.data[index].1)
    }
}

impl<K, V, C: VPtr> Index<C> for OrderedMap<K, V, C> {
    type Output = (K, V);

    fn index(&self, index: C) -> &Self::Output {
        &self.data[index]
    }
}

impl<K, V, C: VPtr> IndexMut<C> for OrderedMap<K, V, C> {
    fn index_mut(&mut self, index: C) -> &mut Self::Output {
        &mut self.data[index]
    }
}

impl<K, V, C: Invalid> Default for OrderedMap<K, V, C> {
    fn default() -> Self {
        Self { 
            index: ShadowMap::new(), 
            data: PoolMap::new(), 
        }
    }
}