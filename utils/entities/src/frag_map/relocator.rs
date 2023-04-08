use std::{
    any::TypeId,
    default::default,
    hash::{BuildHasher, Hash},
    iter, mem,
    ops::{Deref, Range},
    ptr,
    sync::Arc,
    thread,
};

use dashmap::DashMap;
use smallvec::{smallvec, SmallVec};

use crate::*;

use super::{ArcVec, ArcVecInner};

pub trait Unified: Sized + Default {
    fn union(&self, other: &Self) -> Option<Self>;
    fn cmp(&self, other: &Self) -> std::cmp::Ordering;

    fn unify_vec(thread: &mut Vec<Self>) {
        thread.sort_unstable_by(Self::cmp);

        let Some((mut current, mut cursor)) = thread.split_first_mut() else {
            return;
        };
        let mut remining = cursor.len();
        let remine = 'o: loop {
            let len = cursor.len();
            let mut iter = cursor[len - remining..].iter_mut();
            for addr in iter.by_ref() {
                let addr = mem::take(addr);
                let Some(joined) = current.union(&addr) else {
                    remining = iter.len();
                    let len = cursor.len();
                    let Some(split) = cursor.split_first_mut() else {
                        break 'o len;
                    };
                    (current, cursor) = split;
                    *current = addr;
                    continue 'o;
                };
                *current = joined;
            }
            break len;
        };
        let new_len = thread.len() - remine;
        thread.truncate(new_len);
    }
}

pub trait Relocated: Send + Sync {
    fn mark(&self, marker: &mut FragRelocMarker);
    fn remap(&mut self, ctx: &FragMarks) -> Option<()>;
}

impl<'a, T: Relocated + ?Sized> Relocated for &'a mut T {
    fn mark(&self, marker: &mut FragRelocMarker) {
        (**self).mark(marker);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        (**self).remap(ctx)
    }
}

impl<'a, T: Relocated + ?Sized> Relocated for &'a T {
    fn mark(&self, marker: &mut FragRelocMarker) {
        (**self).mark(marker);
    }

    fn remap(&mut self, _ctx: &FragMarks) -> Option<()> {
        None
    }
}

pub trait DynFragMap: Send + Sync {
    fn is_unique(&mut self) -> bool;
    fn mark(&self, addr: FragSliceAddr, marker: &mut FragRelocMarker);
    fn remap(&mut self, ctx: &FragMarks);
    fn filter(&mut self, marks: &mut FragMarkShard);
    fn item_id(&self) -> TypeId;
}

impl<T: Relocated + 'static> DynFragMap for FragBase<T> {
    fn mark(&self, addr: FragSliceAddr, marker: &mut FragRelocMarker) {
        // SAFETY: is_unique should be asserted
        unsafe {
            self.slice_unique(addr).mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragMarks) {
        self.threads
            .iter_mut()
            .flat_map(|t| t.unique_data())
            .for_each(|i| {
                i.remap(ctx);
            })
    }

    fn filter(&mut self, marks: &mut FragMarkShard) {
        marks.filter_base(self.threads.iter_mut().map(|t| {
            (&t.inner, |len| {
                t.len = len;
                t.frozen = len * (t.frozen != 0) as usize;
            })
        }));
    }

    fn is_unique(&mut self) -> bool {
        self.is_unique()
    }

    fn item_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

#[derive(Default)]
pub struct FragRelocator {
    marked: FragMarks,
    markers: Vec<FragRelocMarker>,
}

impl FragRelocator {
    fn clear(&mut self) {
        self.marked.clear();
        self.markers.iter_mut().for_each(FragRelocMarker::clear);
    }

    fn collect_markers(&mut self, thread_count: usize) {
        for (&id, marks) in self.markers.iter_mut().flat_map(|m| m.marked.iter_mut()) {
            self.marked.append(id, marks, thread_count);
        }
    }

    pub fn relocate<T: Relocated>(
        &mut self,
        threads: &mut Vec<Vec<T>>,
        frags: &mut RelocatedObjects,
    ) {
        fn calc_split(len: usize, thread_count: usize) -> usize {
            len / thread_count + (len % thread_count != 0) as usize
        }

        fn chunks<I>(slice: &[I], thread_count: usize) -> impl Iterator<Item = &[I]> {
            let size = calc_split(slice.len(), thread_count);
            (size != 0)
                .then(|| {
                    slice
                        .chunks(size)
                        .chain(iter::repeat(&[][..]))
                        .take(thread_count)
                })
                .into_iter()
                .flatten()
        }

        fn chunks_mut<I>(slice: &mut [I], thread_count: usize) -> impl Iterator<Item = &mut [I]> {
            let size = calc_split(slice.len(), thread_count);
            (size != 0)
                .then(|| {
                    slice
                        .chunks_mut(size)
                        .chain(iter::repeat_with(|| &mut [][..]))
                        .take(thread_count)
                })
                .into_iter()
                .flatten()
        }

        self.clear();

        let threads_len = threads.len();
        self.markers.resize_with(threads_len, default);

        assert!(frags
            .maps
            .iter_mut()
            .flat_map(|(.., m)| m)
            .all(|m| m.is_unique()));

        thread::scope(|scope| {
            let frag_maps = &*frags;
            let iter = self
                .markers
                .iter_mut()
                .zip(threads.iter())
                .zip(chunks(&frags.roots, threads_len))
                .zip(chunks(&frags.static_roots, threads_len));
            for (((marker, thread), roots), statics) in iter {
                scope.spawn(move || {
                    marker.mark_all(statics.iter(), frag_maps);
                    marker.mark_all(thread, frag_maps);
                    marker.mark_all(roots, frag_maps);
                });
            }
        });

        self.collect_markers(threads_len);

        let mut frag_vec = frags
            .maps
            .drain()
            .map(|(id, frag)| (id, frag, self.marked.marks.remove(&id).unwrap_or_default()))
            .collect::<Vec<_>>();

        assert!(self.marked.marks.is_empty(), "{:?}", self.marked.marks);

        thread::scope(|scope| {
            let iter = chunks_mut(&mut frag_vec, threads_len);

            for maps in iter {
                scope.spawn(move || {
                    for (.., frags, marks) in maps {
                        marks.optimize();
                        frags.iter_mut().for_each(|frag| frag.filter(marks));
                    }
                });
            }
        });

        let mut frag_vec = frag_vec
            .into_iter()
            .map(|(id, frag, marks)| {
                self.marked.marks.insert(id, marks);
                (id, frag)
            })
            .collect::<Vec<_>>();

        thread::scope(|scope| {
            let iter = chunks_mut(&mut frag_vec, threads_len)
                .zip(threads.iter_mut())
                .zip(chunks_mut(&mut frags.roots, threads_len))
                .zip(chunks_mut(&mut frags.cleared, threads_len));

            for (((maps, thread), roots), cleared) in iter {
                scope.spawn(|| {
                    for (.., frags) in maps {
                        frags.iter_mut().for_each(|frag| frag.remap(&self.marked));
                    }
                    for root in thread {
                        root.remap(&self.marked);
                    }
                    for root in roots {
                        root.remap(&self.marked);
                    }
                    for clear in cleared {
                        clear.remap(&self.marked);
                    }
                });
            }
        });

        frags.maps.extend(frag_vec.into_iter());
    }
}

#[derive(Default, Debug)]
pub struct FragMarkShard {
    data: Vec<Vec<MarkedRange>>,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct MarkedRange {
    start: u32,
    end: u32,
    reloc_index: u32,
}

impl MarkedRange {
    fn len(&self) -> u32 {
        self.end - self.start
    }
}

impl Unified for MarkedRange {
    fn union(&self, other: &Self) -> Option<Self> {
        let new = Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            reloc_index: 0,
        };

        (self.len() + other.len() >= new.len()).then_some(new)
    }

    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

impl FragMarkShard {
    fn project<T: 'static>(&self, slice: FragSlice<T>) -> Option<FragSlice<T>> {
        let FragSliceAddr { thread, index, len } = slice.0;
        let range_index = self
            .data
            .get(thread as usize)?
            .binary_search_by_key(&index, |r| r.start)
            .map_or_else(|i| i.checked_sub(1), Some)?;
        let marked_range = self.data[thread as usize].get(range_index)?.clone();
        let index = index.checked_sub(marked_range.start)?;
        if index + len as u32 > marked_range.len() {
            return None;
        }
        let index = index + marked_range.reloc_index;
        Some(FragSlice::new(FragSliceAddr { thread, index, len }))
    }

    fn optimize(&mut self) {
        self.data.iter_mut().for_each(Unified::unify_vec);
    }

    pub(crate) fn filter_base<
        T: Relocated + 'static,
        S: FnOnce(usize),
        V: Deref<Target = ArcVec<T>>,
    >(
        &mut self,
        base_threads: impl Iterator<Item = (V, S)>,
    ) {
        for (marks, (thread, len_setter)) in self.data.iter_mut().zip(base_threads) {
            assert!(unsafe { ArcVecInner::is_unique(thread.0) });
            let (base, current_len) = unsafe {
                let slice = ArcVecInner::full_data_mut(thread.0);
                (slice.as_mut_ptr(), slice.len())
            };

            let extras: SmallVec<[Range<u32>; 2]> = match marks.as_slice() {
                [first, .., last] => {
                    smallvec![0..first.start, last.end..current_len as u32]
                }
                [first] => smallvec![0..first.start, first.end..current_len as u32],
                [] => smallvec![0..current_len as u32],
            };

            for range in marks
                .windows(2)
                .map(|s| s[0].end..s[1].start)
                .chain(extras.into_iter().filter(|r| !r.is_empty()))
                .take_while(|r| r.end <= current_len as u32)
            {
                unsafe {
                    ptr::drop_in_place(ptr::slice_from_raw_parts_mut(
                        base.add(range.start as usize),
                        range.len(),
                    ));
                }
            }

            let mut cursor = 0;
            for range in marks.iter_mut().take_while(|r| r.end <= current_len as u32) {
                let len = range.len() as usize;
                unsafe {
                    let source = base.add(range.start as usize);
                    let offset = base.add(cursor);
                    if offset != source {
                        ptr::copy(source, offset, len);
                    }
                    range.reloc_index = cursor as u32;
                    cursor += len;
                }
            }

            len_setter(cursor);
            unsafe {
                field!(thread.0 => mut len) = cursor;
            }
        }
    }

    fn append(&mut self, data: &mut Set<FragSliceAddr>, thread_count: usize) {
        self.data.resize(thread_count, Vec::new());
        for addr in data.drain() {
            let thread = addr.thread as usize;
            self.data[thread].push(MarkedRange {
                start: addr.index,
                end: addr.index + addr.len as u32,
                reloc_index: 0,
            });
        }
    }
}

#[derive(Default)]
pub struct FragMarks {
    marks: Map<TypeId, FragMarkShard>,
}

impl FragMarks {
    pub fn project<T: 'static>(&self, slice: FragSlice<T>) -> Option<FragSlice<T>> {
        self.marks.get(&TypeId::of::<T>())?.project(slice)
    }

    fn append(&mut self, tid: TypeId, data: &mut Set<FragSliceAddr>, thread_count: usize) {
        self.marks
            .entry(tid)
            .or_default()
            .append(data, thread_count);
    }

    fn clear(&mut self) {
        self.marks
            .values_mut()
            .flat_map(|s| &mut s.data)
            .for_each(Vec::clear);
    }
}

#[derive(Default)]
pub struct RelocatedObjects<'a> {
    maps: Map<TypeId, Vec<&'a mut dyn DynFragMap>>,
    roots: Vec<&'a mut dyn Relocated>,
    static_roots: Vec<&'a dyn Relocated>,
    cleared: Vec<&'a mut dyn Relocated>,
}

impl<'a> RelocatedObjects<'a> {
    pub fn add(&mut self, frag_map: &'a mut impl DynFragMap) {
        self.maps
            .entry((*frag_map).item_id())
            .or_default()
            .push(frag_map);
    }

    pub fn add_root(&mut self, root: &'a mut dyn Relocated) {
        self.roots.push(root);
    }

    pub fn add_cleared(&mut self, cleared: &'a mut dyn Relocated) {
        self.cleared.push(cleared);
    }

    pub fn add_static_root(&mut self, root: &'a dyn Relocated) {
        self.static_roots.push(root);
    }

    pub fn clear<'b>(mut self) -> RelocatedObjects<'b> {
        self.maps.clear();
        self.roots.clear();
        unsafe { mem::transmute(self) }
    }
}

#[derive(Default)]
pub struct FragRelocMarker {
    marked: Map<TypeId, Set<FragSliceAddr>>,
    frontier: Vec<(TypeId, FragSliceAddr)>,
}

impl FragRelocMarker {
    pub fn mark_all<'a, T: Relocated + 'a>(
        &mut self,
        roots: impl IntoIterator<Item = &'a T>,
        relocs: &RelocatedObjects,
    ) {
        for root in roots {
            root.mark(self);
        }

        while let Some((type_id, addr)) = self.frontier.pop() {
            for frag_map in relocs.maps.get(&type_id).into_iter().flatten() {
                frag_map.mark(addr, self);
            }
        }
    }

    pub fn is_marked<T: 'static>(&self, frag: FragRef<T>) -> bool {
        self.marked
            .get(&TypeId::of::<T>())
            .filter(|s| s.contains(&frag.0.as_slice()))
            .is_some()
    }

    pub fn mark<T: 'static>(&mut self, frag: FragRef<T>) -> bool {
        if std::any::type_name::<T>().ends_with("Struct") && frag.0.index == 7 {
            panic!()
        }
        self.mark_slice(frag.as_slice())
    }

    pub fn mark_slice<T: 'static>(&mut self, frags: FragSlice<T>) -> bool {
        self.marked
            .entry(TypeId::of::<T>())
            .or_default()
            .insert(frags.0)
            .then(|| self.frontier.push((TypeId::of::<T>(), frags.0)))
            .is_some()
    }

    fn clear(&mut self) {
        self.marked.values_mut().for_each(Set::clear);
        self.frontier.clear();
    }
}

#[macro_export]
macro_rules! derive_relocated {
    (struct $ty:ty { $($field:ident)* }) => {
        #[allow(unused)]
        impl $crate::Relocated for $ty {
            fn mark(&self, marker: &mut $crate::FragRelocMarker) {
                $(self.$field.mark(marker);)*
            }
            fn remap(&mut self, ctx: &$crate::FragMarks) -> Option<()> {
                $(self.$field.remap(ctx)?;)*
                Some(())
            }
        }
    };

    (enum $ty:ty { $($pat:pat => $($field:ident)*,)* }) => {
        impl $crate::Relocated for $ty {
            fn mark(&self, marker: &mut $crate::FragRelocMarker) {
                use $ty::*;
                match self {
                    $($pat => {
                        $($field.mark(marker);)*
                    },)*
                }
            }
            fn remap(&mut self, ctx: &$crate::FragMarks) -> Option<()> {
                use $ty::*;
                match self {
                    $($pat => {
                        $($field.remap(ctx)?;)*
                    },)*
                }
                Some(())
            }
        }
    };
}

impl<T: Relocated, E: Send + Sync + 'static> Relocated for Result<T, E> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        self.as_ref().ok().map(|t| t.mark(marker));
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        self.as_mut().ok().map(|t| t.remap(ctx))?;
        Some(())
    }
}

impl<T: 'static> Relocated for FragRef<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark(*self);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        self.0.index = ctx.project(self.as_slice())?.0.index;
        Some(())
    }
}

impl<T: 'static> Relocated for FragSlice<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark_slice(*self);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        *self = ctx.project(*self)?;
        Some(())
    }
}

impl<A: Relocated, B: Relocated> Relocated for (A, B) {
    fn mark(&self, marker: &mut FragRelocMarker) {
        self.0.mark(marker);
        self.1.mark(marker);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        self.0.remap(ctx)?;
        self.1.remap(ctx)
    }
}

impl<K, V, H> Relocated for DashMap<K, V, H>
where
    K: Relocated + Eq + Hash,
    V: Relocated,
    H: BuildHasher + Sync + Send + Clone,
{
    fn mark(&self, marker: &mut FragRelocMarker) {
        for item in self.iter() {
            item.key().mark(marker);
            item.value().mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        let new_map = DashMap::with_capacity_and_hasher(self.len(), self.hasher().clone());
        for (mut key, mut value) in mem::replace(self, new_map).into_iter() {
            if key.remap(ctx).is_none() || value.remap(ctx).is_none() {
                continue;
            }
            self.insert(key, value);
        }
        Some(())
    }
}

impl<T: Relocated> Relocated for [T] {
    fn mark(&self, marker: &mut FragRelocMarker) {
        for item in self {
            item.mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        for item in self {
            item.remap(ctx)?;
        }

        Some(())
    }
}

impl<T: Relocated> Relocated for Option<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        if let Some(item) = self {
            item.mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        if let Some(item) = self {
            item.remap(ctx)?;
        }

        Some(())
    }
}

macro_rules! impl_relocated_for_deref {
    ($(($($generics:tt)*) for ($($type:tt)*);)*) => {
        $(
            impl<$($generics)*> Relocated for $($type)*
            {
                fn mark(&self, marker: &mut FragRelocMarker) {
                    (**self).mark(marker);
                }

                fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
                    (**self).remap(ctx)
                }
            }
        )*
    };
}

impl_relocated_for_deref!(
    (T: Relocated) for (Vec<T>);
    (T: Relocated) for (SmallVec<[T; 4]>);
    (T: Relocated) for (Box<T>);
);

macro_rules! impl_blanc_relocated {
    ($($ty:ty),*) => {
        $(
            impl Relocated for $ty {
                fn mark(&self, _: &mut FragRelocMarker) {}
                fn remap(&mut self, _: &FragMarks) -> Option<()> { Some(()) }
            }
        )*
    };
}

impl_blanc_relocated! {
    bool, i32
}

impl<T: Relocated> Relocated for Arc<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        (**self).mark(marker);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        let s = Arc::get_mut(self).expect("arc was expected to be unique for remapping");
        s.remap(ctx)
    }
}

#[cfg(test)]
mod test {
    use std::thread;

    use crate::{FragAddr, FragRef, FragRelocator, RelocatedObjects, SyncFragBase};

    #[test]
    fn reloc_stress_test() {
        const THREAD_COUNT: u8 = 4;
        let mut storage = SyncFragBase::new(THREAD_COUNT);
        let mut relocator = FragRelocator::default();

        for mut thread in storage.split() {
            for i in 0..1000 {
                thread.push(vec![i]);
            }
        }

        for j in 0..100 {
            thread::scope(|s| {
                for mut thread in storage.split() {
                    s.spawn(move || {
                        for i in 0..1000 {
                            let tid = i % THREAD_COUNT as usize;
                            let thread_len = thread.base.views[tid]
                                .len
                                .load(std::sync::atomic::Ordering::Relaxed);
                            let index = (i * j) % thread_len;
                            let frag_ref = FragRef::new(FragAddr::new(index as u32, tid as u8));
                            let value = thread[frag_ref].clone();
                            thread.push(value);
                        }
                    });
                }
            });

            let mut maps = RelocatedObjects::default();
            let mut roots = storage
                .views
                .iter()
                .enumerate()
                .map(|(tid, thread)| {
                    (0..thread.len.load(std::sync::atomic::Ordering::Relaxed))
                        .step_by(THREAD_COUNT as usize * 10)
                        .map(|i| FragRef::<Vec<i32>>::new(FragAddr::new(i as u32, tid as u8)))
                        .collect()
                })
                .collect();

            maps.add(&mut storage);
            relocator.relocate(&mut roots, &mut maps);
        }
    }
}
