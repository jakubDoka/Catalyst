use std::ops::{Index, IndexMut};

use cranelift_entity::{PrimaryMap, EntityRef, SecondaryMap};

use crate::gen_entity;

pub struct MetaMap<K: HasMeta, H, M> {
    headers: PrimaryMap<K, H>,
    mapping: SecondaryMap<K, Meta>,
    meta: PrimaryMap<Meta, M>
}

impl<K: HasMeta, H, M> MetaMap<K, H, M> {
    pub fn new() -> Self {
        Self {
            headers: PrimaryMap::new(),
            mapping: SecondaryMap::new(),
            meta: PrimaryMap::new()
        }
    }

    pub fn push_instance(&mut self, header: H, inherits: K) -> K {
        let key = self.headers.push(header);
        self.mapping[key] = self.mapping[inherits];
        key
    }

    pub fn push(&mut self, header: H, meta: M) -> K {
        let key = self.headers.push(header);
        let meta = self.meta.push(meta);
        self.mapping[key] = meta;
        key
    }

    pub fn next_key(&mut self) -> K {
        self.headers.next_key()
    }

    pub fn len(&self) -> usize {
        self.headers.len()
    }
}

impl<K: HasMeta, H, M> Index<K> for MetaMap<K, H, M> {
    type Output = H;

    fn index(&self, key: K) -> &Self::Output {
        &self.headers[key]
    }
}

impl<K: HasMeta, H, M> IndexMut<K> for MetaMap<K, H, M> {
    fn index_mut(&mut self, key: K) -> &mut Self::Output {
        &mut self.headers[key]
    }
}

impl<K: HasMeta, H, M> Index<MetaRef<K>> for MetaMap<K, H, M> {
    type Output = M;

    fn index(&self, key: MetaRef<K>) -> &Self::Output {
        &self.meta[self.mapping[key.0]]
    }
}

impl<K: HasMeta, H, M> IndexMut<MetaRef<K>> for MetaMap<K, H, M> {
    fn index_mut(&mut self, key: MetaRef<K>) -> &mut Self::Output {
        &mut self.meta[self.mapping[key.0]]
    }
}

pub trait HasMeta: EntityRef {
    fn meta(self) -> MetaRef<Self> {
        MetaRef(self)
    }
}

pub struct MetaRef<K: EntityRef>(K);

gen_entity!(Meta);