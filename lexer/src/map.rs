use std::path::Path;

use cranelift_entity::{packed_option::ReservedValue, EntityRef};

pub struct Map<T> {
    lookup: Vec<u32>,
    data: Vec<(ID, T, u32)>,
    free: u32,
}

impl<T: ReservedValue> Map<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            lookup: vec![u32::MAX; Self::best_size(capacity)],
            data: Vec::with_capacity(capacity),
            free: u32::MAX,
        }
    }

    pub fn remove(&mut self, key: impl Into<ID>) -> Option<T> {
        self.remove_by_id(key.into())
    }

    pub fn remove_by_id(&mut self, id: ID) -> Option<T> {
        let index = self.index_of(id);
        let mut current = self.lookup[index];
        let mut last_id = u32::MAX;
        while current != u32::MAX {
            let (identifier, value, next) = &mut self.data[current as usize];

            if *identifier == id && !value.is_reserved_value() {
                let saved_next = *next;
                *next = self.free as u32;
                let value = std::mem::replace(value, T::reserved_value());
                if last_id == u32::MAX {
                    self.lookup[index] = saved_next;
                } else {
                    self.data[last_id as usize].2 = saved_next;
                }
                self.free = current;
                return Some(value);
            }

            last_id = current;
            current = *next;
        }

        None
    }

    pub fn insert(&mut self, key: impl Into<ID>, t: T) -> Option<T> {
        self.insert_by_id(key.into(), t)
    }

    pub fn insert_by_id(&mut self, id: ID, t: T) -> Option<T> {
        let index = self.index_of(id);
        let mut current = self.lookup[index];

        let mut last_id = u32::MAX;

        while current != u32::MAX {
            let (identifier, data, next) = &mut self.data[current as usize];

            if data.is_reserved_value() {
                *identifier = id;
                *data = t;
                return None;
            } else if id == *identifier {
                return Some(std::mem::replace(data, t));
            };

            last_id = current;
            current = *next;
        }

        let new = if self.free == u32::MAX {
            self.data.push((id, t, u32::MAX));
            self.data.len() as u32 - 1
        } else {
            let free = self.free;
            self.free = self.data[free as usize].2;
            self.data[free as usize] = (id, t, u32::MAX);
            free
        };

        if last_id == u32::MAX {
            self.lookup[index] = new;
        } else {
            self.data[last_id as usize].2 = new;
        }

        if self.data.len() > self.lookup.len() {
            self.expand();
        }

        None
    }

    #[cold]
    fn expand(&mut self) {
        let mut new = Self::with_capacity(self.data.len());

        for (id, t, _) in self
            .data
            .drain(..)
            .filter(|(_, t, _)| !t.is_reserved_value())
        {
            new.insert_by_id(id, t);
        }

        *self = new;
    }

    pub fn get(&self, key: impl Into<ID>) -> Option<&T> {
        self.get_by_id(key.into())
    }

    pub fn get_by_id(&self, id: ID) -> Option<&T> {
        let index = self.index_of(id);
        let mut current = self.lookup[index as usize];

        while current != u32::MAX {
            let (ident, data, next) = &self.data[current as usize];
            if *ident == id && !data.is_reserved_value() {
                return Some(data);
            }
            current = *next;
        }

        None
    }

    pub fn iter(&self) -> impl Iterator<Item = (ID, &T)> {
        self.data.iter().map(|(id, t, _)| (*id, t))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (ID, &mut T)> {
        self.data.iter_mut().map(|(id, t, _)| (*id, t))
    }

    pub fn clear(&mut self) {
        self.lookup.iter_mut().for_each(|x| *x = u32::MAX);
        self.data.clear();
        self.free = u32::MAX;
    }

    fn index_of(&self, ident: ID) -> usize {
        ident.0 as usize & (self.lookup.len() - 1)
    }

    fn best_size(current: usize) -> usize {
        current.next_power_of_two()
    }
}

impl<T> Default for Map<T> {
    fn default() -> Self {
        Self {
            lookup: vec![u32::MAX],
            data: Vec::new(),
            free: u32::MAX,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ID(pub u64);

impl ID {
    pub fn new(data: &str) -> Self {
        Self::from_bytes(data.as_bytes())
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

    #[test]
    fn test_id() {
        use super::ID;

        let id = ID::new("hello");
        let id2 = ID::from_path(Path::new("hello"));
        let id3 = ID::new("world");

        assert_eq!(id, id2);
        assert_ne!(id, id3);
    }
}
