use std::{
    marker::PhantomData,
    mem::replace,
    ops::{Index, IndexMut},
};

use serde::{
    de::{MapAccess, Visitor},
    ser::SerializeMap,
    Deserialize, Deserializer, Serialize,
};

use crate::{Maybe, VPtr};

/// Storage plugin that supports data reuse and optional storage.
pub struct SparseMap<K, T> {
    mapping: Vec<Maybe<Private>>,
    data: Vec<(K, T)>,
}

impl<K, T> SparseMap<K, T> {
    pub fn new() -> Self {
        SparseMap {
            mapping: Vec::new(),
            data: Vec::new(),
        }
    }
}

impl<K: VPtr, T> SparseMap<K, T> {
    /// Inserts new value under the key.
    ///
    /// # Example
    /// ```
    /// let mut map = storage::SparseMap::<Dummy, usize>::new();
    ///
    /// map.insert(Dummy(0), 10);
    /// assert_eq!(map[Dummy(0)], 10);
    /// assert_eq!(map.len(), 1);
    ///
    /// assert_eq!(map.insert(Dummy(0), 20), Some(10));
    ///
    /// storage::gen_v_ptr!(Dummy);
    /// ```
    pub fn insert(&mut self, key: K, value: T) -> Option<T> {
        if let Some(key) = self.project(key).expand() {
            Some(replace(&mut self.data[key.index()].1, value))
        } else {
            let id = Private::new(self.data.len());
            let size = self.mapping.len().max(key.index() + 1);
            self.mapping.resize(size, Maybe::none());
            self.mapping[key.index()] = Maybe::some(id);
            self.data.push((key, value));
            None
        }
    }

    /// Removes value under the key. Former value is returned if any.
    ///
    /// # Example
    /// ```
    /// let mut map = storage::SparseMap::<Dummy, usize>::new();
    ///
    /// assert_eq!(map.remove(Dummy(0)), None);
    /// map.insert(Dummy(0), 10);
    /// assert_eq!(map.remove(Dummy(0)), Some(10));
    ///
    /// storage::gen_v_ptr!(Dummy);
    /// ```
    pub fn remove(&mut self, key: K) -> Option<T> {
        self.project(key).expand().map(|private| {
            self.mapping[key.index()] = Maybe::none();
            let last_id = self.data.last().unwrap().0;
            self.mapping[last_id.index()] = private.into();
            self.data.swap_remove(private.index()).1
        })
    }

    pub fn get(&self, key: K) -> Option<&T> {
        self.project(key)
            .expand()
            .map(|key| &self.data[key.index()].1)
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut T> {
        self.project(key)
            .expand()
            .map(|key| &mut self.data[key.index()].1)
    }

    fn project(&self, key: K) -> Maybe<Private> {
        let index = key.index();
        *self.mapping.get(index).unwrap_or(&Maybe::none())
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn size_hint(&self) -> usize {
        self.mapping.len()
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.data.iter().map(|(_, value)| value)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data.iter_mut().map(|(_, value)| value)
    }

    pub fn keys(&self) -> impl DoubleEndedIterator<Item = K> + ExactSizeIterator<Item = K> + '_ {
        self.data.iter().map(|&(key, _)| key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &T)> {
        self.data.iter().map(|(key, value)| (*key, value))
    }
}

impl<K: VPtr, T> Index<K> for SparseMap<K, T> {
    type Output = T;

    fn index(&self, key: K) -> &T {
        assert!(key.index() < self.mapping.len());
        let inner = self.mapping[key.index()].unwrap().index();
        assert!(inner < self.data.len());
        &self.data[inner].1
    }
}

impl<K: VPtr, T> IndexMut<K> for SparseMap<K, T> {
    fn index_mut(&mut self, key: K) -> &mut T {
        assert!(key.index() < self.mapping.len());
        let inner = self.mapping[key.index()].unwrap().index();
        assert!(inner < self.data.len());
        &mut self.data[inner].1
    }
}

impl<K, T> Default for SparseMap<K, T> {
    fn default() -> Self {
        Self::new()
    }
}

gen_v_ptr!(Private);

impl<K: VPtr + Serialize, T: Serialize> Serialize for SparseMap<K, T>
where
    K: Serialize,
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let len = (self.mapping.len() << 32) | self.data.len();
        let mut map = serializer.serialize_map(Some(len))?;
        for (key, value) in self.data.iter() {
            map.serialize_entry(key, value)?;
        }
        map.end()
    }
}

struct SparseMapVisitor<K, V> {
    marker: PhantomData<fn() -> SparseMap<K, V>>,
}

impl<K, V> SparseMapVisitor<K, V> {
    fn new() -> Self {
        SparseMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, K, V> Visitor<'de> for SparseMapVisitor<K, V>
where
    K: Deserialize<'de> + VPtr,
    V: Deserialize<'de>,
{
    type Value = SparseMap<K, V>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("SparseMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let size_hint = access.size_hint().unwrap_or(0);
        let (mapping_len, value_len) = (size_hint >> 32, size_hint & 0xFFFFFFFF);

        let mut map = SparseMap {
            mapping: Vec::with_capacity(mapping_len),
            data: Vec::with_capacity(value_len),
        };

        // While there are entries remaining in the input, add them
        // into our map.
        while let Some((key, value)) = access.next_entry()? {
            map.insert(key, value);
        }

        Ok(map)
    }
}

impl<'de, K, V> Deserialize<'de> for SparseMap<K, V>
where
    K: Deserialize<'de> + VPtr,
    V: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(SparseMapVisitor::new())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sparse_map_serde() {
        let mut map = SparseMap::<Private, usize>::new();
        map.insert(Private::new(0), 10);
        map.insert(Private::new(20), 20);
        map.insert(Private::new(30), 30);

        let bytes = rmp_serde::to_vec(&map).unwrap();

        let map2: SparseMap<Private, usize> = rmp_serde::from_slice(&bytes[..]).unwrap();

        assert_eq!(map2.get(Private::new(0)), Some(&10));
        assert_eq!(map2.get(Private::new(20)), Some(&20));
        assert_eq!(map2.get(Private::new(30)), Some(&30));
    }
}
