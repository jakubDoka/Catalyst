use std::ops::{Index, IndexMut};

use crate::*;

/// Has access complexity of an ordinary map, but it allows addressing
/// values by [`VPtr`]
pub struct PartialOrderedMap<V, C> {
    index: Map<C>,
    data: PoolMap<C, (Maybe<Ident>, V)>,
}

impl<V, C: VPtr> PartialOrderedMap<V, C> {
    pub fn redirect_insert(&mut self, key: Ident, value: V) -> C {
        let index = self.data.push((key.into(), value));
        self.index.insert(key, index);
        index
    }

    /// Inserts a new value into the map returning its possible shadow and [`VPtr`] to it.   
    pub fn insert(&mut self, key: Ident, value: V) -> (C, Option<V>) {
        let index = self.data.push((key.into(), value));
        let shadow = self.index.insert(key, index);
        (index, shadow.map(|shadow| self.data.remove(shadow).1))
    }

    pub fn push(&mut self, value: V) -> C {
        self.data.push((Maybe::none(), value))
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
        assert!(found_key == Maybe::some(key));
        Some(value)
    }

    pub fn remove_index(&mut self, index: C) -> (Maybe<Ident>, V) {
        let (key, value) = self.data.remove(index);
        if let Some(key) = key.expand() {
            self.index
                .remove(key)
                .expect("index should be present as long as value is present");
        }
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

    pub fn id(&self, key: C) -> Maybe<Ident> {
        self.data[key].0
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.values().map(|(_, value)| value)
    }
}

impl<V, C: VPtr> Index<C> for PartialOrderedMap<V, C> {
    type Output = V;

    fn index(&self, index: C) -> &Self::Output {
        &self.data[index].1
    }
}

impl<V, C: VPtr> IndexMut<C> for PartialOrderedMap<V, C> {
    fn index_mut(&mut self, index: C) -> &mut Self::Output {
        &mut self.data[index].1
    }
}

impl<V, C: Invalid> Default for PartialOrderedMap<V, C> {
    fn default() -> Self {
        Self {
            index: Map::new(),
            data: PoolMap::new(),
        }
    }
}

impl<V, C: Invalid> PartialOrderedMap<V, C> {
    pub fn new() -> Self {
        Self::default()
    }
}
