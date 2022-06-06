use std::{
    collections::HashMap,
    fmt::Debug,
    hash::{BuildHasher, Hash, Hasher},
    ops::{Deref, DerefMut, Index, IndexMut},
    path::Path,
};

use cranelift_entity::{packed_option::ReservedValue, EntityRef};

use crate::BitSerde;

pub struct SecondaryMap<K: EntityRef, V: Default + Clone> {
    inner: cranelift_entity::SecondaryMap<K, V>,
}

impl<K: EntityRef, V: Default + Clone> SecondaryMap<K, V> {
    pub fn new() -> Self {
        SecondaryMap {
            inner: cranelift_entity::SecondaryMap::new(),
        }
    }

    pub fn with_capacity(len: usize) -> Self {
        SecondaryMap {
            inner: cranelift_entity::SecondaryMap::with_capacity(len),
        }
    }
}

impl<K: EntityRef, V: Default + Clone> Index<K> for SecondaryMap<K, V> {
    type Output = V;

    #[inline(always)]
    fn index(&self, key: K) -> &Self::Output {
        &self.inner[key]
    }
}

impl<K: EntityRef, V: Default + Clone> IndexMut<K> for SecondaryMap<K, V> {
    #[inline(always)]
    fn index_mut(&mut self, key: K) -> &mut Self::Output {
        assert!(key.index() < u32::MAX as usize);
        &mut self.inner[key]
    }
}

impl<K: EntityRef, V: Default + Clone> Deref for SecondaryMap<K, V> {
    type Target = cranelift_entity::SecondaryMap<K, V>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<K: EntityRef, V: Default + Clone> DerefMut for SecondaryMap<K, V> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<K: EntityRef, V: Default + Clone + BitSerde + PartialEq + Eq> BitSerde for SecondaryMap<K, V> {
    fn write(&self, buffer: &mut Vec<u8>) {
        let default = V::default();
        self.inner
            .values()
            .filter(|&i| i != &default)
            .count()
            .write(buffer);
        for (k, item) in self.inner.iter() {
            if item != &default {
                k.index().write(buffer);
                item.write(buffer);
            }
        }
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        let len = usize::read(cursor, buffer)?;

        if len + (std::mem::size_of::<usize>() + std::mem::size_of::<V>()) > buffer.len() {
            return Err(format!(
                "SecondaryMap::read: buffer too small: {} < {}",
                buffer.len(),
                len + (std::mem::size_of::<usize>() * 2)
            ));
        }

        let mut map = SecondaryMap::with_capacity(len);
        for _ in 0..len {
            let key = usize::read(cursor, buffer)?;
            let item = V::read(cursor, buffer)?;
            map[K::new(key)] = item;
        }
        Ok(map)
    }
}

#[derive(Debug, Clone)]
pub struct SimpleHasher {
    state: u64,
}

impl Hasher for SimpleHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.state
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        let mut data = [0u8; 8];
        data.copy_from_slice(bytes);
        self.state = u64::from_le_bytes(data);
    }
}

#[derive(Debug, Clone, Default)]
struct BH;

impl BuildHasher for BH {
    type Hasher = SimpleHasher;

    fn build_hasher(&self) -> Self::Hasher {
        SimpleHasher { state: 0 }
    }
}

pub struct Set {
    map: Map<()>,
}

impl Set {
    pub fn new() -> Self {
        Set { map: Map::new() }
    }

    pub fn with_capacity(len: usize) -> Self {
        Set {
            map: Map::with_capacity(len),
        }
    }

    pub fn insert(&mut self, key: impl Into<ID>) -> bool {
        self.map.insert(key, ()).is_none()
    }

    pub fn remove(&mut self, key: impl Into<ID>) -> bool {
        self.map.remove(key).is_some()
    }

    pub fn contains(&self, key: impl Into<ID>) -> bool {
        self.map.contains_key(key)
    }

    pub fn iter(&self) -> impl Iterator<Item = ID> + '_ {
        self.map.iter().map(|(k, _)| k)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Map<T> {
    inner: HashMap<ID, T, BH>,
}

impl<T> Map<T> {
    pub fn new() -> Self {
        Map {
            inner: HashMap::with_hasher(BH),
        }
    }

    pub fn with_capacity(len: usize) -> Self {
        Map {
            inner: HashMap::with_capacity_and_hasher(len, BH),
        }
    }

    pub fn insert(&mut self, id: impl Into<ID>, value: T) -> Option<T> {
        self.inner.insert(id.into(), value)
    }

    pub fn insert_unique(&mut self, id: impl Into<ID>, value: T) {
        assert!(self.insert(id, value).is_none());
    }

    pub fn remove(&mut self, id: impl Into<ID>) -> Option<T> {
        self.inner.remove(&id.into())
    }

    pub fn get(&self, id: impl Into<ID>) -> Option<&T> {
        self.inner.get(&id.into())
    }

    pub fn get_mut(&mut self, id: impl Into<ID>) -> Option<&mut T> {
        self.inner.get_mut(&id.into())
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ID, &T)> {
        self.inner.iter().map(|(&k, v)| (k, v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (ID, &mut T)> {
        self.inner.iter_mut().map(|(&k, v)| (k, v))
    }

    pub fn contains_key(&self, key: impl Into<ID>) -> bool {
        self.inner.contains_key(&key.into())
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn drain(&mut self) -> impl Iterator<Item = (ID, T)> + '_ {
        self.inner.drain()
    }
}

impl<T: BitSerde> BitSerde for Map<T> {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.inner.len().write(buffer);
        for (k, v) in self.inner.iter() {
            k.write(buffer);
            v.write(buffer);
        }
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        let len = usize::read(cursor, buffer)?;
        if len * (std::mem::size_of::<ID>() + std::mem::size_of::<T>())
            > buffer.len() + std::mem::align_of::<T>() * len
        {
            return Err(format!(
                "Map::read: buffer too small: {} < {}",
                buffer.len() + std::mem::align_of::<T>() * len,
                len * (std::mem::size_of::<ID>() + std::mem::size_of::<T>())
            ));
        }

        let mut map = Map::with_capacity(len);
        for _ in 0..len {
            let key = ID::read(cursor, buffer)?;
            let value = T::read(cursor, buffer)?;
            map.insert_unique(key, value);
        }

        Ok(map)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ID(pub u64);

impl ID {
    pub fn to_ident(&self, buffer: &mut String) {
        let mut current = self.0;
        let radix = 36;
        while current > 0 {
            buffer.push(char::from_digit((current % radix) as u32, radix as u32).unwrap());
            current /= radix;
        }
    }

    pub fn from_ident(ident: &str) -> Self {
        let mut current = 0u64;
        let radix = 36;
        for c in ident.chars().rev() {
            current *= radix;
            current += c.to_digit(radix as u32).unwrap() as u64;
        }
        ID(current)
    }

    pub fn new(data: &str) -> Self {
        Self::from_bytes(data.as_bytes())
    }

    pub fn bound_impl(bound: Self, implementor: Self) -> Self {
        Self::new("<impl>") + bound + implementor
    }

    pub fn pointer(ty: Self) -> Self {
        Self::new("*") + ty
    }

    pub fn owned(ty: Self, name: Self) -> Self {
        ty + name
    }

    pub fn scoped(item: Self, scope: impl EntityRef + IDFilter) -> Self {
        Self::from(scope) + item
    }

    pub fn bound_impl_func(bound: Self, implementor: Self, func: Self) -> Self {
        implementor + Self::owned(bound, func)
    }

    pub fn binary(left: Self, op: Self) -> Self {
        Self::new("<binary>") + left + op
    }

    pub fn unary(ty: Self, op: Self) -> Self {
        Self::new("<unary>") + ty + op
    }

    pub fn from_path(path: &Path) -> Self {
        Self::from_bytes(unsafe { std::mem::transmute(path) })
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        Self(bytes.iter().fold(0, |acc, &b| {
            acc.wrapping_add(acc << 16)
                .wrapping_add(acc << 8)
                .wrapping_sub(b as u64)
        }))
    }
}

impl Hash for ID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(&self.0.to_le_bytes());
    }
}

impl ReservedValue for ID {
    fn reserved_value() -> Self {
        Self(u64::MAX)
    }

    fn is_reserved_value(&self) -> bool {
        self.0 == u64::MAX
    }
}

impl From<&str> for ID {
    fn from(str: &str) -> Self {
        Self::new(str)
    }
}

impl From<&Path> for ID {
    fn from(path: &Path) -> Self {
        Self::from_path(path)
    }
}

impl From<&[u8]> for ID {
    fn from(bytes: &[u8]) -> Self {
        Self::from_bytes(bytes)
    }
}

pub trait IDFilter {}

impl<T: EntityRef + IDFilter> From<T> for ID {
    fn from(entity: T) -> Self {
        Self(entity.index() as u64)
    }
}

impl std::ops::Add<ID> for ID {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        ID(self.0
            ^ (0x9e3779b9u64
                .wrapping_add(self.0 << 6)
                .wrapping_add(self.0 >> 2)
                .wrapping_add(other.0)))
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;

    use super::ID;

    #[test]
    fn test_id() {
        use super::ID;

        let id = ID::new("hello");
        let id2 = ID::from_path(Path::new("hello"));
        let id3 = ID::new("world");

        assert_eq!(id, id2);
        assert_ne!(id, id3);
    }

    #[test]
    fn test_map() {
        // test all map functions
        let mut map = super::Map::new();

        let stride = 10000;
        let passes = 10;

        fn map_id(i: usize) -> ID {
            ID(((i * i - i) * i) as u64)
        }

        let iter = (1..stride).map(map_id);

        for _ in 0..passes {
            for i in iter.clone() {
                map.insert(i, i);
            }

            for i in iter.clone() {
                assert_eq!(map.get(i), Some(&i));
            }

            for i in iter.clone() {
                assert_eq!(map.remove(i), Some(i));
            }
        }

        panic!();
    }
}
