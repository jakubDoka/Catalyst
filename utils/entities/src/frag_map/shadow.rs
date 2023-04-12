use std::{default::default, marker::PhantomData, mem, sync::Arc};

use crate::{
    field, frag_map::ArcVecInner, DynFragMap, FragMap, FragRef, NoInteriorMutability, Relocated,
};

use super::ArcVec;

pub struct ShadowFragMap<K, T> {
    base: ShadowFragBase<K, T>,
    local: ShadowFragView<T>,
}

impl<K, T> ShadowFragMap<K, T> {
    pub fn get(&self, index: FragRef<K>, backing: &FragMap<K>) -> &T
    where
        T: NoInteriorMutability,
    {
        if index.0.thread == self.local.thread {
            unsafe { ArcVecInner::full_data(self.local.data.0) }
                .get(index.0.index as usize)
                .unwrap_or(&self.base.default)
        } else {
            self.base.get(index, backing)
        }
    }

    pub fn get_mut(&mut self, index: FragRef<K>, backing: &FragMap<K>) -> &mut T
    where
        T: Default,
    {
        let thread = &mut self.local;
        let backing_thread = &backing.thread_local;
        assert!(index.0.index as usize >= backing_thread.frozen);
        let len = unsafe { field!(thread.data.0 => len) };

        thread
            .data
            .extend((len..index.0.index as usize + 1).map(|_| default()));
        unsafe { &mut *ArcVecInner::get_item(thread.data.0, index.0.index as usize) }
    }

    pub fn pull(&mut self, base: &ShadowFragBase<K, T>) {
        self.base.threads.clone_from_slice(&base.threads);
    }

    pub fn commit(&self, base: &mut ShadowFragBase<K, T>) {
        base.threads[self.local.thread as usize] = self.local.clone();
    }

    pub fn commit_unique(self, base: &mut ShadowFragBase<K, T>) {
        let thread = self.local.thread;
        base.threads[thread as usize] = self.local;
    }
}

pub struct ShadowFragBase<K, T> {
    threads: Box<[ShadowFragView<T>]>,
    default: Arc<T>,
    phantom: PhantomData<fn(K)>,
}

impl<K, T> ShadowFragBase<K, T> {
    pub fn new(thread_count: u8) -> Self
    where
        T: Default,
    {
        Self {
            threads: (0..thread_count).map(ShadowFragView::new).collect(),
            default: default(),
            phantom: PhantomData,
        }
    }

    pub fn expand(&mut self, thread_count: u8) {
        let threads = (self.threads.len() as u8..thread_count).map(ShadowFragView::new);
        let current = mem::take(&mut self.threads);
        self.threads = current.into_vec().into_iter().chain(threads).collect();
    }

    pub fn split(&self) -> impl Iterator<Item = ShadowFragMap<K, T>> + '_ {
        self.threads.iter().map(|thread| ShadowFragMap {
            base: self.clone(),
            local: thread.clone(),
        })
    }

    pub fn is_unique(&mut self) -> bool {
        Arc::get_mut(&mut self.default).is_some()
    }

    fn get_low(&self, index: FragRef<K>, backing: &FragMap<K>) -> Option<&T> {
        let thread = self.threads.get(index.0.thread as usize)?;
        let backing_thread = backing.others.get(index.0.thread as usize)?;
        assert!((index.0.index as usize) < backing_thread.frozen);
        let len = unsafe { field!(thread.data.0 => len) };
        let border = len.min(backing_thread.frozen);
        unsafe { ArcVecInner::data(thread.data.0, border) }.get(index.0.index as usize)
    }

    pub fn get(&self, index: FragRef<K>, backing: &FragMap<K>) -> &T
    where
        T: NoInteriorMutability,
    {
        self.get_low(index, backing).unwrap_or(&self.default)
    }
}

impl<K: 'static, T: Relocated + 'static> DynFragMap for ShadowFragBase<K, T> {
    fn is_unique(&mut self) -> bool {
        self.is_unique()
    }

    fn mark(&self, addr: crate::FragSliceAddr, marker: &mut crate::FragRelocMarker) {
        let thread = &self.threads[addr.thread as usize];
        unsafe {
            ArcVecInner::full_data(thread.data.0)
                .get(addr.index as usize..addr.index as usize + addr.len as usize)
                .mark(marker);
        }
    }

    fn remap(&mut self, ctx: &crate::FragMarks) {
        self.threads
            .iter_mut()
            .flat_map(|thread| unsafe { ArcVecInner::full_data_mut(thread.data.0) })
            .for_each(|item| {
                item.remap(ctx);
            });
    }

    fn filter(&mut self, marks: &mut super::relocator::FragMarkShard) {
        marks.filter_base(self.threads.iter_mut().map(|thread| (&thread.data, |_| {})))
    }

    fn item_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<K>()
    }
}

impl<K, T> Clone for ShadowFragBase<K, T> {
    fn clone(&self) -> Self {
        Self {
            threads: self.threads.clone(),
            default: self.default.clone(),
            phantom: PhantomData,
        }
    }
}

pub struct ShadowFragView<T> {
    data: ArcVec<T>,
    thread: u8,
}

unsafe impl<T: Send> Send for ShadowFragView<T> {}
unsafe impl<T: Sync> Sync for ShadowFragView<T> {}

impl<T> ShadowFragView<T> {
    fn new(thread: u8) -> Self {
        Self {
            data: ArcVec::new(),
            thread,
        }
    }
}

impl<T> Clone for ShadowFragView<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            thread: self.thread,
        }
    }
}
