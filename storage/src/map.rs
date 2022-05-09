use std::{
    fmt::Debug,
    ops::{Deref, DerefMut, Index, IndexMut},
    path::Path, collections::HashMap, hash::{Hasher, Hash, BuildHasher},
};

use cranelift_entity::{packed_option::ReservedValue, EntityRef};

pub struct SecondaryMap<K: EntityRef, V: Default + Clone> {
    inner: cranelift_entity::SecondaryMap<K, V>,
}

impl<K: EntityRef, V: Default + Clone> SecondaryMap<K, V> {
    pub fn new() -> Self {
        SecondaryMap {
            inner: cranelift_entity::SecondaryMap::new(),
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

#[derive(Debug, Clone)]
pub struct Map<T> {
    inner: HashMap<ID, T, BH>,
}

impl<T> Map<T> {
    pub fn new() -> Self {
        Map {
            inner: HashMap::with_hasher(BH),
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

    pub fn clear(&mut self) {
        self.inner.clear()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ID(pub u64);

impl ID {
    pub fn new(data: &str) -> Self {
        Self::from_bytes(data.as_bytes())
    }

    pub fn raw_field(ty: Self, field: u64) -> Self {
        ty + Self(field)
    }

    pub fn bound_impl(bound: Self, implementor: Self) -> Self {
        Self::new("<impl>") + bound + implementor
    }

    pub fn pointer(ty: Self) -> Self {
        Self::new("*") + ty
    }

    pub fn field(ty: Self, name: Self) -> Self {
        ty + name
    }

    pub fn bound_impl_owned_func(bound: Self, implementor: Self, func: Self) -> Self {
        implementor + Self::owned_func(bound, func)
    }

    pub fn binary(left: Self, op: Self) -> Self {
        Self::new("<binary>") + left + op
    }

    pub fn owned_func(ty: Self, name: Self) -> Self {
        ty + name
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

macro_rules! strip_plus {
    (+ $($s:tt)*) => {
        $($s)*
    };
}

macro_rules! impl_from_for_tuples {
    ($($($t:ident),*);*) => {
        $(
            #[allow(non_snake_case)]
            impl<$($t: Into<ID>),*> From<($($t),*)> for ID {
                fn from(($($t),*): ($($t),*)) -> Self {
                    strip_plus!($(+ $t.into())*)
                }
            }
        )*
    };
}

impl_from_for_tuples!(
    A, B;
    A, B, C;
    A, B, C, D
);

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