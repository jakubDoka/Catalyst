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

use crate::*;

/// Storage plugin that supports data reuse and optional storage.
pub struct SparseMap<T, V = T> {
    mapping: Vec<u32>,
    data: Vec<(VRef<T>, V)>,
}

impl<T, V> SparseMap<T, V> {
    pub fn new() -> Self {
        SparseMap {
            mapping: Vec::new(),
            data: Vec::new(),
        }
    }
}

impl<T, V> SparseMap<T, V> {
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
    pub fn insert(&mut self, key: VRef<T>, value: V) -> Option<V> {
        if let Some(key) = self.project(key) {
            Some(replace(&mut self.data[key as usize].1, value))
        } else {
            let id = self.data.len() as u32;
            let size = self.mapping.len().max(key.index() + 1);
            self.mapping.resize(size, u32::MAX);
            self.mapping[key.index()] = id;
            self.data.push((key, value));
            None
        }
    }

    pub fn insert_unique(&mut self, key: VRef<T>, value: V) {
        assert!(self.insert(key, value).is_none());
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
    pub fn remove(&mut self, key: VRef<T>) -> Option<V> {
        self.project(key).map(|private| {
            self.mapping[key.index()] = u32::MAX;
            let last_id = self.data.last().unwrap().0;
            if last_id == key {
                self.data.pop().unwrap().1
            } else {
                self.mapping[last_id.index()] = private;
                self.data.swap_remove(private as usize).1
            }
        })
    }

    pub fn get(&self, key: VRef<T>) -> Option<&V> {
        self.project(key).map(|key| &self.data[key as usize].1)
    }

    pub fn get_mut(&mut self, key: VRef<T>) -> Option<&mut V> {
        self.project(key).map(|key| &mut self.data[key as usize].1)
    }

    fn project(&self, key: VRef<T>) -> Option<u32> {
        let index = key.index();
        self.mapping
            .get(index)
            .copied()
            .filter(|&id| id != u32::MAX)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn size_hint(&self) -> usize {
        self.mapping.len()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.iter().map(|(_, value)| value)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut().map(|(_, value)| value)
    }

    pub fn keys(
        &self,
    ) -> impl DoubleEndedIterator<Item = VRef<T>> + ExactSizeIterator<Item = VRef<T>> + '_ {
        self.data.iter().map(|&(key, _)| key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<T>, &V)> {
        self.data.iter().map(|(key, value)| (*key, value))
    }

    pub fn clear(&mut self) {
        self.mapping.clear();
        self.data.clear();
    }
}

impl<T, V> Index<VRef<T>> for SparseMap<T, V> {
    type Output = V;

    fn index(&self, key: VRef<T>) -> &V {
        assert!(key.index() < self.mapping.len());
        let inner = self.mapping[key.index()] as usize;
        assert!(inner < self.data.len());
        &self.data[inner].1
    }
}

impl<T, V> IndexMut<VRef<T>> for SparseMap<T, V> {
    fn index_mut(&mut self, key: VRef<T>) -> &mut V {
        assert!(key.index() < self.mapping.len());
        let inner = self.mapping[key.index()] as usize;
        assert!(inner < self.data.len());
        &mut self.data[inner].1
    }
}

impl<T, V> Default for SparseMap<T, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, V: Serialize> Serialize for SparseMap<T, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let len = (self.mapping.len() << 32) | self.data.len();
        let mut map = serializer.serialize_map(Some(len))?;
        for (key, value) in self.data.iter() {
            map.serialize_entry(&key.index(), value)?;
        }
        map.end()
    }
}

struct SparseMapVisitor<T, V> {
    marker: PhantomData<fn() -> SparseMap<T, V>>,
}

impl<T, V> SparseMapVisitor<T, V> {
    fn new() -> Self {
        SparseMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, T, V> Visitor<'de> for SparseMapVisitor<T, V>
where
    V: Deserialize<'de>,
{
    type Value = SparseMap<T, V>;

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
            map.insert(unsafe { VRef::new(key) }, value);
        }

        Ok(map)
    }
}

impl<'de, T, V> Deserialize<'de> for SparseMap<T, V>
where
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
        let mut map = SparseMap::<usize>::new();
        unsafe {
            map.insert(VRef::new(0), 10);
            map.insert(VRef::new(20), 20);
            map.insert(VRef::new(30), 30);
        }

        let bytes = rmp_serde::to_vec(&map).unwrap();

        let map2: SparseMap<usize> = rmp_serde::from_slice(&bytes[..]).unwrap();

        unsafe {
            assert_eq!(map2.get(VRef::new(0)), Some(&10));
            assert_eq!(map2.get(VRef::new(20)), Some(&20));
            assert_eq!(map2.get(VRef::new(30)), Some(&30));
        }
    }
}
