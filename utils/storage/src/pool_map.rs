use std::{
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Index, IndexMut, Not},
};

use serde::{
    de::{MapAccess, Visitor},
    ser::SerializeMap,
    Deserialize, Deserializer, Serialize, Serializer,
};

use crate::{BitSet, VRef};

/// Supports reusable storage via stack base allocator. It performs extra
/// checks for debug builds but is unsafe on release.
pub struct PoolMap<K, T = K> {
    free: Vec<VRef<K>>,
    free_lookup: BitSet,
    data: Vec<MaybeUninit<T>>,
    phantom: PhantomData<fn(K) -> K>,
}

impl<K, V: Clone> Clone for PoolMap<K, V> {
    fn clone(&self) -> Self {
        PoolMap {
            free: self.free.clone(),
            free_lookup: self.free_lookup.clone(),
            data: self
                .data
                .iter()
                .enumerate()
                .map(|(i, value)| {
                    if self.free_lookup.contains(i) {
                        MaybeUninit::uninit()
                    } else {
                        MaybeUninit::new(unsafe { &*value.as_ptr() }.clone())
                    }
                })
                .collect(),
            phantom: PhantomData,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.free.clone_from(&source.free);
        self.free_lookup.clone_from(&source.free_lookup);
        self.data.clear();
        self.data
            .extend(source.data.iter().enumerate().map(|(i, value)| {
                if source.free_lookup.contains(i) {
                    MaybeUninit::uninit()
                } else {
                    MaybeUninit::new(unsafe { &*value.as_ptr() }.clone())
                }
            }));
        self.phantom = PhantomData;
    }
}

impl<K, V> PoolMap<K, V> {
    /// Does not allocate.
    pub fn new() -> Self {
        PoolMap {
            free: Vec::new(),
            free_lookup: BitSet::new(),
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
            assert!(self.free_lookup.remove(key.index()));
            self[key] = value;
            key
        } else {
            self.data.push(MaybeUninit::new(value));
            unsafe { VRef::new(self.data.len() - 1) }
        }
    }

    /// Pushes new value to map and returns it's key. Value of key is deterministic
    /// but arbitrary. Previously removed slots can be reused.
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
        assert!(self.free_lookup.insert(key.index()));

        let elem = &mut self.data[key.index()];
        let value = std::mem::replace(elem, MaybeUninit::uninit());
        self.free.push(key);

        // SAFETY: Asserted at the beginning of function
        unsafe { value.assume_init() }
    }

    pub fn next(&self) -> VRef<K> {
        self.free
            .last()
            .copied()
            .unwrap_or_else(|| unsafe { VRef::new(self.data.len()) })
    }

    pub fn size_hint(&self) -> usize {
        self.data.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<K>, &V)> {
        self.data.iter().enumerate().filter_map(|(i, elem)| {
            self.free_lookup
                .contains(i)
                .not()
                .then(|| unsafe { (VRef::new(i), &*elem.as_ptr()) })
        })
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }

    pub fn clear(&mut self) {
        self.free.clear();
        for (i, mut elem) in self.data.drain(..).enumerate() {
            if !self.free_lookup.contains(i) {
                // SAFETY: If implies that element was initialized
                unsafe { elem.assume_init_drop() };
            }
        }
        self.free_lookup.clear();
    }
}

impl<K, V> Serialize for PoolMap<K, V>
where
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let size = (self.free.len() << 32) | (self.data.len() - self.free.len());
        let mut map = serializer.serialize_map(Some(size))?;
        let iter = self
            .data
            .iter()
            .enumerate()
            .filter(|&(k, _)| !self.free_lookup.contains(k));
        for (k, v) in iter {
            map.serialize_entry::<usize, V>(&k, unsafe { &*v.as_ptr() })?;
        }
        map.end()
    }
}

impl<K, V> Drop for PoolMap<K, V> {
    fn drop(&mut self) {
        self.clear();
    }
}

struct PoolMapVisitor<K, V> {
    marker: PhantomData<fn() -> PoolMap<K, V>>,
}

impl<K, V> PoolMapVisitor<K, V> {
    fn new() -> Self {
        PoolMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, K, V> Visitor<'de> for PoolMapVisitor<K, V>
where
    V: Deserialize<'de>,
{
    type Value = PoolMap<K, V>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("PoolMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let size_hint = access.size_hint().unwrap_or(0);
        let (free_len, value_len) = (size_hint >> 32, size_hint & 0xFFFFFFFF);

        let mut map = PoolMap {
            free: Vec::with_capacity(free_len),
            free_lookup: BitSet::with_capacity(free_len),
            data: Vec::with_capacity(value_len),
            phantom: PhantomData,
        };

        // While there are entries remaining in the input, add them
        // into our map.
        for _ in 0..value_len {
            let (key, value) = access.next_entry::<usize, V>()?.unwrap();
            while map.data.len() < key {
                map.free.push(unsafe { VRef::new(map.data.len()) });
                map.free_lookup.insert(map.data.len());
                map.data.push(MaybeUninit::uninit());
            }
            map.data.push(MaybeUninit::new(value));
        }

        Ok(map)
    }
}

impl<'de, K, V> Deserialize<'de> for PoolMap<K, V>
where
    V: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(PoolMapVisitor::new())
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
        assert!(!self.free_lookup.contains(key.index()));
        // SAFETY: Just asserted.
        unsafe { &*self.data[key.index()].as_ptr() }
    }
}

impl<K, V> IndexMut<VRef<K>> for PoolMap<K, V> {
    fn index_mut(&mut self, key: VRef<K>) -> &mut V {
        assert!(!self.free_lookup.contains(key.index()));
        // SAFETY: Just asserted.
        unsafe { &mut *self.data[key.index()].as_mut_ptr() }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pool_map_serde() {
        let mut map = PoolMap::<usize>::new();
        let ten = map.push(10);
        let twenty = map.push(20);
        let thirty = map.push(30);
        map.remove(twenty);

        let ser = rmp_serde::to_vec(&map).unwrap();
        let mut map2: PoolMap<usize> = rmp_serde::from_slice(&ser).unwrap();

        assert_eq!(map2[ten], 10);
        assert_eq!(map2[thirty], 30);
        assert_eq!(map2.push(20), twenty);
    }
}
