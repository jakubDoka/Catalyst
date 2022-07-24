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

use crate::{VPtr, VPtrSet};

/// Supports reusable storage via stack base allocator. It performs extra
/// checks for debug builds but is unsafe on release.
pub struct PoolMap<K, V> {
    free: Vec<K>,
    free_lookup: VPtrSet<K>,
    data: Vec<MaybeUninit<V>>,

    _ph: PhantomData<fn(K) -> K>,
}

impl<K, V> PoolMap<K, V> {
    /// Does not allocate.
    pub fn new() -> Self {
        PoolMap {
            free: Vec::new(),
            free_lookup: VPtrSet::new(),
            data: Vec::new(),
            _ph: PhantomData,
        }
    }
}

impl<K: VPtr, V> PoolMap<K, V> {
    /// Pushes new value to map and returns it's key. Value of key is deterministic
    /// but arbitrary. Previously removed slots can be reused.
    ///
    /// # Example
    /// ```
    /// let mut map = storage::PoolMap::<Dummy, usize>::new();
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
    /// storage::gen_v_ptr!(Dummy);
    pub fn push(&mut self, value: V) -> K {
        let key = if let Some(key) = self.free.pop() {
            assert!(self.free_lookup.remove(key));
            self[key] = value;
            key
        } else {
            self.data.push(MaybeUninit::new(value));
            K::new(self.data.len() - 1)
        };

        key
    }

    /// Pushes new value to map and returns it's key. Value of key is deterministic
    /// but arbitrary. Previously removed slots can be reused.
    ///
    /// # Example
    ///
    /// ```
    /// let mut map = storage::PoolMap::<Dummy, usize>::new();
    /// let ten = map.push(10);
    /// let twenty = map.push(20);
    ///
    /// assert_eq!(map.remove(ten), 10);
    /// assert_eq!(map.remove(twenty), 20);
    /// assert_eq!(map.push(0), twenty);
    /// assert_eq!(map.push(0), ten);
    ///
    /// storage::gen_v_ptr!(Dummy);
    pub fn remove(&mut self, key: K) -> V {
        assert!(self.free_lookup.insert(key));

        let elem = &mut self.data[key.index()];
        let value = std::mem::replace(elem, MaybeUninit::uninit());
        self.free.push(key);

        // SAFETY: Asserted at the beginning of function
        unsafe { value.assume_init() }
    }

    pub fn size_hint(&self) -> usize {
        self.data.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.data.iter().enumerate().filter_map(|(i, elem)| {
            self.free_lookup
                .contains(K::new(i))
                .not()
                .then(|| (K::new(i), unsafe { &*elem.as_ptr() }))
        })
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }
}

impl<K, V> Serialize for PoolMap<K, V>
where
    K: Serialize + VPtr,
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
            .map(|(i, elem)| (K::new(i), elem))
            .filter(|&(k, _)| !self.free_lookup.contains(k));
        for (k, v) in iter {
            map.serialize_entry::<K, V>(&k, unsafe { &*v.as_ptr() })?;
        }
        map.end()
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
    K: Deserialize<'de> + VPtr,
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
            free_lookup: VPtrSet::with_capacity(free_len),
            data: Vec::with_capacity(value_len),
            _ph: PhantomData,
        };

        // While there are entries remaining in the input, add them
        // into our map.
        for _ in 0..value_len {
            let (key, value) = access.next_entry::<K, V>()?.unwrap();
            while map.data.len() < key.index() {
                map.free.push(K::new(map.data.len()));
                map.free_lookup.insert(K::new(map.data.len()));
                map.data.push(MaybeUninit::uninit());
            }
            map.data.push(MaybeUninit::new(value));
        }

        Ok(map)
    }
}

impl<'de, K, V> Deserialize<'de> for PoolMap<K, V>
where
    K: Deserialize<'de> + VPtr,
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

impl<K: VPtr, V> Index<K> for PoolMap<K, V> {
    type Output = V;

    fn index(&self, key: K) -> &V {
        assert!(!self.free_lookup.contains(key));
        // SAFETY: Just asserted.
        unsafe { &*self.data[key.index()].as_ptr() }
    }
}

impl<K: VPtr, V> IndexMut<K> for PoolMap<K, V> {
    fn index_mut(&mut self, key: K) -> &mut V {
        assert!(!self.free_lookup.contains(key));
        // SAFETY: Just asserted.
        unsafe { &mut *self.data[key.index()].as_mut_ptr() }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pool_map_serde() {
        let mut map = PoolMap::<Dummy, usize>::new();
        let ten = map.push(10);
        let twenty = map.push(20);
        let thirty = map.push(30);
        map.remove(twenty);

        let ser = rmp_serde::to_vec(&map).unwrap();
        let mut map2: PoolMap<Dummy, usize> = rmp_serde::from_slice(&ser).unwrap();

        assert_eq!(map2[ten], 10);
        assert_eq!(map2[thirty], 30);
        assert_eq!(map2.push(20), twenty);
    }

    gen_v_ptr!(Dummy);
}
