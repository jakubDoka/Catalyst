use std::{
    default::default,
    ops::{Index, IndexMut},
};

use crate::*;

/// Has access complexity of an ordinary map, but it allows addressing
/// values by [`VPtr`]
pub struct PartialOrderedMap<K: SpecialHash, V> {
    index: Map<K, VRef<V>>,
    data: PoolMap<V, (Option<K>, V)>,
}

impl<K: SpecialHash, V> PartialOrderedMap<K, V> {
    pub fn redirect_insert(&mut self, key: K, value: V) -> VRef<V> {
        let index = self.data.push((key.into(), value));
        self.index.insert(key, index);
        index
    }

    /// Inserts a new value into the map returning its possible shadow and [`VPtr`] to it.
    pub fn insert(&mut self, key: K, value: V) -> (VRef<V>, Option<V>) {
        let index = self.data.push((key.into(), value));
        let shadow = self.index.insert(key, index);
        (index, shadow.map(|shadow| self.data.remove(shadow).1))
    }

    pub fn push(&mut self, value: V) -> VRef<V> {
        self.data.push((None, value))
    }

    /// Inserts a new value, panicking on shadow and returning [`VPtr`] to it.
    pub fn insert_unique(&mut self, key: K, value: V) -> VRef<V> {
        let (index, shadow) = self.insert(key, value);
        assert!(shadow.is_none());
        index
    }

    pub fn remove(&mut self, key: K) -> Option<V> {
        let index = self.index.remove(&key)?;
        let (found_key, value) = self.data.remove(index);
        assert!(found_key == Some(key));
        Some(value)
    }

    pub fn remove_index(&mut self, index: VRef<V>) -> (Option<K>, V) {
        let (key, value) = self.data.remove(index);
        if let Some(key) = key {
            self.index
                .remove(&key)
                .expect("index should be present as long as value is present");
        }
        (key, value)
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.index.get(&key).map(|&index| &self.data[index].1)
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.index.get(&key).map(|&index| &mut self.data[index].1)
    }

    pub fn index(&self, key: K) -> Option<VRef<V>> {
        self.index.get(&key).copied()
    }

    pub fn id(&self, key: VRef<V>) -> Option<K> {
        self.data[key].0
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.values().map(|(_, value)| value)
    }
}

impl<K: SpecialHash, V> Index<VRef<V>> for PartialOrderedMap<K, V> {
    type Output = V;

    fn index(&self, index: VRef<V>) -> &Self::Output {
        &self.data[index].1
    }
}

impl<K: SpecialHash, V> IndexMut<VRef<V>> for PartialOrderedMap<K, V> {
    fn index_mut(&mut self, index: VRef<V>) -> &mut Self::Output {
        &mut self.data[index].1
    }
}

impl<K: SpecialHash, V> Default for PartialOrderedMap<K, V> {
    fn default() -> Self {
        Self {
            index: default(),
            data: PoolMap::new(),
        }
    }
}

impl<K: SpecialHash, V> PartialOrderedMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }
}
