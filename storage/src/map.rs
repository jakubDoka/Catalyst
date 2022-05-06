use std::{
    fmt::Debug,
    ops::{Deref, DerefMut, Index, IndexMut},
    path::Path,
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

#[derive(Clone, Debug)]
pub struct Map<T> {
    lookup: Vec<MapStorageNode>,
    storage: MapStorage<T>,
}

impl<T: Default + Clone> Map<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            lookup: vec![MapStorageNode::reserved_value(); Self::best_size(capacity)],
            storage: MapStorage::with_capacity(capacity),
        }
    }

    pub fn remove(&mut self, key: impl Into<ID>) -> Option<T> {
        self.remove_by_id(key.into())
    }

    pub fn remove_by_id(&mut self, id: ID) -> Option<T> {
        let index = self.index_of(id);
        let node = self.lookup[index];
        let current = self.storage.get_mut(node);
        let local_index = current.iter().position(|(o_id, _)| *o_id == id)?;
        let new_len = current.len() - 1;
        current.swap(local_index, new_len);
        let ret = std::mem::take(&mut current[new_len].1);
        self.lookup[index] = self.storage.resize(node, new_len);
        Some(ret)
    }

    pub fn insert_unique(&mut self, key: impl Into<ID>, value: T) {
        assert!(self.insert_by_id(key.into(), value).is_none());
    }

    pub fn insert(&mut self, key: impl Into<ID>, t: T) -> Option<T> {
        self.insert_by_id(key.into(), t)
    }

    pub fn insert_by_id(&mut self, id: ID, t: T) -> Option<T> {
        let index = self.index_of(id);
        let node = self.lookup[index];
        let current = self.storage.get_mut(node);
        if let Some((_, duplicate)) = current.iter_mut().find(|(o_id, _)| *o_id == id) {
            return Some(std::mem::replace(duplicate, t));
        }

        let new_len = current.len() + 1;
        let new_node = self.storage.resize(node, new_len);
        *self.storage.get_mut(new_node).last_mut().unwrap() = (id, t);
        self.lookup[index] = new_node;

        if self.storage.len() > self.lookup.len() / 2 {
            self.expand();
        }

        None
    }

    #[cold]
    fn expand(&mut self) {
        let mut new = Self::with_capacity(self.storage.len() * 2);

        for &node in self.lookup.iter().filter(|n| !n.is_reserved_value()) {
            self.storage.get_mut(node).iter_mut().for_each(|node| {
                let (id, node) = std::mem::take(node);
                new.insert_by_id(id, node);
            });
        }

        *self = new;
    }

    pub fn get(&self, key: impl Into<ID>) -> Option<&T> {
        self.get_by_id(key.into())
    }

    pub fn get_by_id(&self, id: ID) -> Option<&T> {
        let index = self.index_of(id);
        let current = self.storage.get(self.lookup[index]);

        current
            .iter()
            .find_map(|(o_id, value)| (*o_id == id).then(|| value))
    }

    pub fn clear(&mut self) {
        self.lookup
            .iter_mut()
            .for_each(|x| *x = MapStorageNode::reserved_value());
        self.storage.clear();
    }

    fn index_of(&self, ident: ID) -> usize {
        ident.0 as usize & (self.lookup.len() - 1)
    }

    fn best_size(current: usize) -> usize {
        current.next_power_of_two()
    }

    pub fn collision_rate(&self) -> f64 {
        let mut collisions = 0;
        let mut total = 0;
        for &node in self.lookup.iter().filter(|n| !n.is_reserved_value()) {
            let current = self.storage.get(node);
            let len = current.len();
            collisions += len - 1;
            total += len;
        }

        collisions as f64 / total as f64
    }
}

impl<T> Default for Map<T> {
    fn default() -> Self {
        Self {
            lookup: vec![MapStorageNode::reserved_value()],
            storage: MapStorage::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MapStorage<T> {
    data: Vec<(ID, T)>,
    free: Vec<u32>,
}

impl<T> MapStorage<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            free: Vec::new(),
        }
    }
}

impl<T: Default + Clone> MapStorage<T> {
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn get(&self, node: MapStorageNode) -> &[(ID, T)] {
        if node.is_reserved_value() {
            return &[];
        }
        &self.data[node.start as usize..node.end as usize]
    }

    pub fn get_mut(&mut self, node: MapStorageNode) -> &mut [(ID, T)] {
        if node.is_reserved_value() {
            return &mut [];
        }
        &mut self.data[node.start as usize..node.end as usize]
    }

    pub fn alloc(&mut self, size: usize) -> MapStorageNode {
        if size == 0 {
            return MapStorageNode::reserved_value();
        }

        let free_slot = self
            .free
            .get(size - 1)
            .and_then(|&x| (x != u32::MAX).then_some(x));

        if let Some(free_slot) = free_slot {
            self.free[size - 1] = MapStorageNode::from_id(self.data[free_slot as usize].0).end;
            MapStorageNode {
                start: free_slot,
                end: free_slot + size as u32,
            }
        } else {
            let start = self.data.len();
            let end = start + size;
            self.data.resize(end, Default::default());
            MapStorageNode {
                start: start as u32,
                end: end as u32,
            }
        }
    }

    pub fn resize(&mut self, node: MapStorageNode, new_len: usize) -> MapStorageNode {
        let new = self.alloc(new_len);

        for (i, j) in
            (node.start as usize..node.end as usize).zip(new.start as usize..new.end as usize)
        {
            self.data[j] = std::mem::take(&mut self.data[i]);
        }

        self.dealloc(node);

        new
    }

    pub fn dealloc(&mut self, node: MapStorageNode) {
        if node.is_reserved_value() {
            return;
        }

        let size = (node.end - node.start) as usize - 1;
        self.free.resize(self.free.len().max(size + 1), u32::MAX);
        self.data[node.start as usize].0 = MapStorageNode {
            start: u32::MAX,
            end: self.free[size],
        }
        .to_id();
        self.free[size] = node.start;
    }

    fn with_capacity(capacity: usize) -> MapStorage<T> {
        MapStorage {
            data: Vec::with_capacity(capacity),
            free: vec![],
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.free.clear();
    }
}

#[derive(Copy, Clone, Debug)]
pub struct MapStorageNode {
    start: u32,
    end: u32,
}

impl ReservedValue for MapStorageNode {
    fn reserved_value() -> Self {
        Self {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.start == u32::MAX
    }
}

impl MapStorageNode {
    pub fn to_id(&self) -> ID {
        ID(self.start as u64 | ((self.end as u64) << 32))
    }

    pub fn from_id(id: ID) -> Self {
        Self {
            start: id.0 as u32,
            end: (id.0 >> 32) as u32,
        }
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

            println!("{}", map.storage.len());
        }

        panic!();
    }
}
