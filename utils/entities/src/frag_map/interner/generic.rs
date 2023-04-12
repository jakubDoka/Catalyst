use std::{default::default, sync::Arc};

use crate::*;

pub trait Interned: Sized {
    type Id: Copy + Eq + std::hash::Hash + Send + Sync + 'static;
    type Context;

    fn id_with_ctx(&self, ctx: Self::Context) -> Self::Id;

    fn id(&self) -> Self::Id
    where
        Self::Context: Default,
    {
        self.id_with_ctx(default())
    }
}

pub struct GenericInternerBase<T: Interned> {
    map: Arc<CMap<T::Id, FragRef<T>>>,
    storage: SyncFragBase<T>,
}

impl<T: Interned> Default for GenericInternerBase<T> {
    fn default() -> Self {
        Self {
            map: default(),
            storage: default(),
        }
    }
}

impl<T: Interned> GenericInternerBase<T> {
    pub fn adjust(&mut self, thread_count: u8) {
        self.storage.adjust(thread_count);
    }

    pub fn split(&mut self) -> impl Iterator<Item = GenericInterner<T>> + '_ {
        self.storage.split().map(|storage| GenericInterner {
            map: self.map.clone(),
            storage,
        })
    }
}

pub struct GenericInterner<T: Interned> {
    map: Arc<CMap<T::Id, FragRef<T>>>,
    pub storage: SyncFragMap<T>,
}

impl<T: Interned> GenericInterner<T> {
    pub fn intern(&mut self, value: T) -> FragRef<T>
    where
        T::Context: Default,
    {
        self.intern_with_ctx(value, default())
    }

    pub fn intern_with_ctx(&mut self, value: T, ctx: T::Context) -> FragRef<T> {
        let id = value.id_with_ctx(ctx);
        *self
            .map
            .entry(id)
            .or_insert_with(|| self.storage.push(value))
    }
}
