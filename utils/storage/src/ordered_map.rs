use std::ops::{Index, IndexMut};

use crate::*;

/// Has access complexity of an ordinary map, but it allows addressing
/// values by [`VPtr`]
pub struct OrderedMap<V, C> {
    index: Map<C>,
    data: PoolMap<C, (Ident, V)>,
}

impl<V, C: VPtr> OrderedMap<V, C> {
    /// Inserts a new value into the map returning its possible shadow and [`VPtr`] to it.   
    pub fn insert(&mut self, key: Ident, value: V) -> (C, Option<V>) {
        let index = self.data.push((key, value));
        let shadow = self.index.remove(key);
        (index, shadow.map(|shadow| self.data.remove(shadow).1))
    }

    /// Inserts a new value, panicking on shadow and returning [`VPtr`] to it.
    pub fn insert_unique(&mut self, key: Ident, value: V) -> C {
        let (index, shadow) = self.insert(key, value);
        assert!(shadow.is_none());
        index
    }

    pub fn remove(&mut self, key: Ident) -> Option<V> {
        let index = self.index.remove(key)?;
        let (found_key, value) = self.data.remove(index);
        assert!(found_key == key);
        Some(value)
    }

    pub fn remove_index(&mut self, index: C) -> (Ident, V) {
        let (key, value) = self.data.remove(index);
        self.index
            .remove(key)
            .expect("index should be present as long as value is present");
        (key, value)
    }

    pub fn get(&self, key: Ident) -> Option<&V> {
        self.index.get(key).map(|&index| &self.data[index].1)
    }

    pub fn get_mut(&mut self, key: Ident) -> Option<&mut V> {
        self.index.get(key).map(|&index| &mut self.data[index].1)
    }

    pub fn index(&self, key: Ident) -> Option<C> {
        self.index.get(key).copied()
    }

    pub fn id(&self, key: C) -> Ident {
        self.data[key].0
    }
}

impl<V, C: VPtr> Index<C> for OrderedMap<V, C> {
    type Output = V;

    fn index(&self, index: C) -> &Self::Output {
        &self.data[index].1
    }
}

impl<V, C: VPtr> IndexMut<C> for OrderedMap<V, C> {
    fn index_mut(&mut self, index: C) -> &mut Self::Output {
        &mut self.data[index].1
    }
}

impl<V, C: Invalid> Default for OrderedMap<V, C> {
    fn default() -> Self {
        Self {
            index: Map::new(),
            data: PoolMap::new(),
        }
    }
}

impl<V, C: Invalid> OrderedMap<V, C> {
    pub fn new() -> Self {
        Self::default()
    }
}
