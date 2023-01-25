use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use rkyv::{Archive, Deserialize, Serialize};

use crate::VRef;

/// Supports reusable storage via stack base allocator. It performs extra
/// checks for debug builds but is unsafe on release.
#[derive(Deserialize, Serialize, Archive)]

pub struct PoolMap<K, T = K> {
    free: Vec<VRef<K>>,
    data: Vec<Option<T>>,
    phantom: PhantomData<fn(K) -> K>,
}

impl<K, V: Clone> Clone for PoolMap<K, V> {
    fn clone(&self) -> Self {
        PoolMap {
            free: self.free.clone(),
            data: self.data.clone(),
            phantom: PhantomData,
        }
    }
}

impl<K, V> PoolMap<K, V> {
    /// Does not allocate.
    pub fn new() -> Self {
        PoolMap {
            free: Vec::new(),
            data: Vec::new(),
            phantom: PhantomData,
        }
    }
}

impl<K, V> PoolMap<K, V> {
    /// Pushes new value to map and returns it's key. Value of key is deterministic
    /// but arbitrary. Previously removed slots can be reused.
    ///
    /// # Example
    /// ```
    /// let mut map = storage::PoolMap::<usize>::new();
    ///
    /// let ten = map.push(10);
    /// assert_eq!(map[ten], 10);
    ///
    /// let twenty = map.push(20);
    /// assert_ne!(ten, twenty);
    ///
    /// let removed_ten = map.remove(ten);
    /// assert_eq!(removed_ten, 10);
    ///
    /// // this can happen
    /// let restored_ten = map.push(10);
    /// assert_eq!(ten, restored_ten);
    /// assert_eq!(map[restored_ten], 10);
    ///
    /// ```
    pub fn push(&mut self, value: V) -> VRef<K> {
        if let Some(key) = self.free.pop() {
            self.data[key.index()] = Some(value);
            key
        } else {
            self.data.push(Some(value));
            VRef::new(self.data.len() - 1)
        }
    }

    ///
    /// TODO: add document this
    ///
    /// # Example
    ///
    /// ```
    /// let mut map = storage::PoolMap::<usize>::new();
    /// let ten = map.push(10);
    /// let twenty = map.push(20);
    ///
    /// assert_eq!(map.remove(ten), 10);
    /// assert_eq!(map.remove(twenty), 20);
    /// assert_eq!(map.push(0), twenty);
    /// assert_eq!(map.push(0), ten);
    /// ```
    pub fn remove(&mut self, key: VRef<K>) -> V {
        let elem = self.data[key.index()].take().expect("double free");
        self.free.push(key);
        elem
    }

    pub fn next(&self) -> VRef<K> {
        self.free
            .last()
            .copied()
            .unwrap_or_else(|| VRef::new(self.data.len()))
    }

    pub fn size_hint(&self) -> usize {
        self.data.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<K>, &V)> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(i, elem)| Some((VRef::new(i), elem.as_ref()?)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (VRef<K>, &mut V)> {
        self.data
            .iter_mut()
            .enumerate()
            .filter_map(|(i, elem)| Some((VRef::new(i), elem.as_mut()?)))
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.iter().flatten()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut().flatten()
    }

    pub fn clear(&mut self) {
        self.free.clear();
        self.data.clear();
    }

    pub fn retain(&mut self, mut predicate: impl FnMut(&mut V) -> bool) {
        for (i, v) in self.data.iter_mut().enumerate() {
            if let Some(val) = v && !predicate(val) {
                v.take();
                self.free.push(VRef::new(i));
            }
        }
    }
}

impl<K, V> Default for PoolMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> Index<VRef<K>> for PoolMap<K, V> {
    type Output = V;

    fn index(&self, key: VRef<K>) -> &V {
        self.data[key.index()]
            .as_ref()
            .expect("accessing uninit slot")
    }
}

impl<K, V> IndexMut<VRef<K>> for PoolMap<K, V> {
    fn index_mut(&mut self, key: VRef<K>) -> &mut V {
        self.data[key.index()]
            .as_mut()
            .expect("accessing uninit slot")
    }
}
