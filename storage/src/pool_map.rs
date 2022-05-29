use std::ops::{Index, IndexMut};

use cranelift_entity::{packed_option::ReservedValue, EntityRef, PrimaryMap};

use crate::BitSerde;

pub struct PoolMap<K: EntityRef, V: ReservedValue> {
    pub data: PrimaryMap<K, V>,
    pub free: Vec<K>,
}

impl<K: EntityRef, V: ReservedValue> PoolMap<K, V> {
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    fn with_capacity(len: usize) -> Self {
        Self {
            data: PrimaryMap::with_capacity(len),
            free: Vec::new(),
        }
    }

    pub fn push(&mut self, value: V) -> K {
        debug_assert!(!value.is_reserved_value());
        if let Some(free) = self.free.pop() {
            self.data[free] = value;
            free
        } else {
            self.data.push(value)
        }
    }

    pub fn remove(&mut self, e: K) -> V {
        debug_assert!(!self.data[e].is_reserved_value());
        self.free.push(e);
        std::mem::replace(&mut self.data[e], V::reserved_value())
    }
}

impl<E: EntityRef, T: ReservedValue> Index<E> for PoolMap<E, T> {
    type Output = T;

    fn index(&self, e: E) -> &T {
        assert!(!self.data[e].is_reserved_value());
        &self.data[e]
    }
}

impl<E: EntityRef, T: ReservedValue> IndexMut<E> for PoolMap<E, T> {
    fn index_mut(&mut self, e: E) -> &mut T {
        assert!(!self.data[e].is_reserved_value());
        &mut self.data[e]
    }
}

impl<K: EntityRef, T: BitSerde + ReservedValue> BitSerde for PoolMap<K, T> {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.data.len().write(buffer);
        self.free.len().write(buffer);
        for item in self.data.values() {
            if item.is_reserved_value() {
                false.write(buffer);
            } else {
                true.write(buffer);
                item.write(buffer);
            }
        }
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        let len = usize::read(cursor, buffer)?;
        let free_len = usize::read(cursor, buffer)?;

        if len * std::mem::size_of::<bool>() + (len - free_len) * std::mem::size_of::<T>()
            > buffer.len()
        {
            return Err(format!(
                "PoolMap length {} * {} + {} * {} exceeds buffer length {}",
                len,
                std::mem::size_of::<bool>(),
                len - free_len,
                std::mem::size_of::<T>(),
                buffer.len()
            ));
        }

        let mut map = PoolMap::with_capacity(len);
        map.free.reserve(free_len);

        for _ in 0..len {
            let is_reserved = bool::read(cursor, buffer)?;
            if is_reserved {
                map.free.push(K::new(map.data.len()));
                map.data.push(T::reserved_value());
            } else {
                map.data.push(T::read(cursor, buffer)?);
            }
        }

        Ok(map)
    }
}
