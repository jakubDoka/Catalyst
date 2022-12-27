use std::{
    alloc::{Allocator, Global},
    default::default,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use bump_alloc::{bumpvec, BumpAllocRef};
// use serde::{de::Visitor, ser::SerializeSeq, Deserialize, Deserializer, Serialize};

use crate::VRef;

#[derive(Debug)]
pub struct ShadowMap<T, V, A: Allocator = Global> {
    data: Vec<V, A>,
    default: V,
    phantom: PhantomData<fn(T) -> T>,
}

impl<T, V: Default> ShadowMap<T, V> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T, V: Default> ShadowMap<T, V, BumpAllocRef> {
    pub fn bump(cap: usize) -> Self {
        Self {
            data: bumpvec![cap cap].into(),
            default: V::default(),
            phantom: PhantomData,
        }
    }
}

impl<T, V: Default, A: Allocator> ShadowMap<T, V, A> {
    pub fn clear(&mut self)
    where
        V: Clone,
    {
        self.data.fill(self.default.clone());
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<T>, &V)> {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| (unsafe { VRef::new(i) }, v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (VRef<T>, &mut V)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (unsafe { VRef::new(i) }, v))
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut()
    }
}

impl<T, V, A: Allocator> Index<VRef<T>> for ShadowMap<T, V, A> {
    type Output = V;

    fn index(&self, key: VRef<T>) -> &V {
        self.data.get(key.index()).unwrap_or(&self.default)
    }
}

impl<T, V: Default + Clone, A: Allocator> IndexMut<VRef<T>> for ShadowMap<T, V, A> {
    fn index_mut(&mut self, key: VRef<T>) -> &mut V {
        let index = key.index();
        self.data.resize(self.data.len().max(index + 1), default());
        &mut self.data[index]
    }
}

impl<T, V: Default> Default for ShadowMap<T, V> {
    fn default() -> Self {
        ShadowMap {
            data: Vec::new(),
            default: V::default(),
            phantom: PhantomData,
        }
    }
}

impl<T, V: Clone> Clone for ShadowMap<T, V> {
    fn clone(&self) -> Self {
        ShadowMap {
            data: self.data.clone(),
            default: self.default.clone(),
            phantom: PhantomData,
        }
    }
}

// struct ShadowMapVisitor<K, V> {
//     marker: PhantomData<fn() -> ShadowMap<K, V>>,
// }

// impl<K, V> ShadowMapVisitor<K, V> {
//     fn new() -> Self {
//         ShadowMapVisitor {
//             marker: PhantomData,
//         }
//     }
// }

// impl<'de, K, V: Default + Clone> Visitor<'de> for ShadowMapVisitor<K, V>
// where
//     V: Deserialize<'de>,
// {
//     type Value = ShadowMap<K, V>;

//     fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
//         formatter.write_str("ShadowMap")
//     }

//     fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
//     where
//         A: serde::de::SeqAccess<'de>,
//     {
//         let mut map = ShadowMap::new();
//         while let Some(value) = seq.next_element::<(VRef<K>, V)>()? {
//             map[value.0] = value.1;
//         }
//         Ok(map)
//     }
// }

// impl<'de, K, V: Default + Clone> Deserialize<'de> for ShadowMap<K, V>
// where
//     V: Deserialize<'de>,
// {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         deserializer.deserialize_seq(ShadowMapVisitor::new())
//     }
// }

// impl<K, V: Default + Clone + PartialEq + Eq> Serialize for ShadowMap<K, V>
// where
//     V: Serialize,
// {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: serde::Serializer,
//     {
//         let mut seq = serializer.serialize_seq(None)?;
//         // we do this in reverse to guarantee that deserialization will allocate just once
//         for (i, v) in self.data.iter().enumerate().rev() {
//             if *v != self.default {
//                 seq.serialize_element(&(unsafe { VRef::<K>::new(i) }, v))?;
//             }
//         }
//         seq.end()
//     }
// }
