use std::{
    alloc::Allocator,
    any::TypeId,
    default::default,
    hash::{BuildHasher, Hash},
    iter, mem,
    ops::Deref,
    ptr,
    sync::Arc,
    thread,
};

use dashmap::DashMap;
use smallvec::SmallVec;

use crate::*;

use super::{FragVecArc, FragVecInner};

pub trait Relocated: Send + Sync {
    fn mark(&self, marker: &mut FragRelocMarker);
    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()>;
}

impl<'a, T: Relocated + ?Sized> Relocated for &'a mut T {
    fn mark(&self, marker: &mut FragRelocMarker) {
        (**self).mark(marker);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
        (**self).remap(ctx)
    }
}

impl<'a, T: Relocated + ?Sized> Relocated for &'a T {
    fn mark(&self, marker: &mut FragRelocMarker) {
        (**self).mark(marker);
    }

    fn remap(&mut self, _ctx: &FragRelocMapping) -> Option<()> {
        Some(())
    }
}

#[derive(Default)]
pub struct FragRelocMapping {
    marked: Map<DynFragId, DynFragId>,
    edges: Map<(TypeId, u8), usize>,
}

impl FragRelocMapping {
    pub fn project<T: 'static>(&self, frag: FragRef<T>) -> Option<FragRef<T>> {
        self.marked
            .get(&DynFragId::new(frag))
            .and_then(|id| id.downcast())
            .or_else(|| {
                let (index, thread) = frag.parts();
                let &guard = self.edges.get(&(TypeId::of::<T>(), thread))?;
                (index as usize <= guard).then_some(frag)
            })
    }

    pub fn project_slice<T: 'static>(&self, slice: FragSlice<T>) -> FragSlice<T> {
        let Some(start) = slice.keys().next() else {
            return FragSlice::empty();
        };

        let Some(projected) = self.project(start) else {
            return FragSlice::empty();
        };

        let (index, thread) = projected.parts();

        FragSlice::new(FragSliceAddr::new(index, thread, slice.len() as u16))
    }

    fn clear(&mut self) {
        self.marked.clear();
        self.edges.clear();
    }
}

pub trait DynFragMap: Send + Sync {
    fn is_unique(&self) -> bool;
    fn mark(&self, addr: FragAddr, marker: &mut FragRelocMarker);
    fn remap(&mut self, ctx: &FragRelocMapping);
    fn filter(&mut self, marks: &mut FragMarks, mapping: &mut FragRelocMapping);
    fn item_id(&self) -> TypeId;
}

impl<T: Relocated + 'static, A: Allocator + Send + Sync> DynFragMap for FragBase<T, A> {
    fn mark(&self, addr: FragAddr, marker: &mut FragRelocMarker) {
        let addr = FragRef::<T>::new(addr);
        // SAFETY: is_unique should be asserted
        unsafe {
            self.index_unique(addr).mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        self.threads
            .iter_mut()
            .map(|t| t.unique_data())
            .flatten()
            .for_each(|i| {
                i.remap(ctx);
            })
    }

    fn filter(&mut self, marks: &mut FragMarks, mapping: &mut FragRelocMapping) {
        marks.filter_base(
            self.threads.iter_mut().map(|t| {
                (&t.inner, |len| {
                    t.len = len;
                    t.frozen = len * (t.frozen != 0) as usize;
                })
            }),
            mapping,
        );
    }

    fn is_unique(&self) -> bool {
        self.is_unique()
    }

    fn item_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

#[derive(Default)]
pub struct FragRelocator {
    marked: Map<TypeId, FragMarks>,
    markers: Vec<FragRelocMarker>,
    mapping: Vec<FragRelocMapping>,
}

impl FragRelocator {
    fn clear(&mut self) {
        self.marked.values_mut().for_each(FragMarks::clear);
        self.markers.iter_mut().for_each(FragRelocMarker::clear);
        self.mapping.iter_mut().for_each(FragRelocMapping::clear);
    }

    fn collect_markers(&mut self, thread_count: usize) {
        for marker in self.markers.iter_mut() {
            for (&id, marks) in marker.marked.iter_mut() {
                self.marked
                    .entry(id)
                    .or_default()
                    .append(marks, thread_count);
            }
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
            slice
                .chunks(size)
                .chain(iter::repeat(&[][..]))
                .take(thread_count)
        }

        fn chunks_mut<I>(slice: &mut [I], thread_count: usize) -> impl Iterator<Item = &mut [I]> {
            let size = calc_split(slice.len(), thread_count);
            slice
                .chunks_mut(size)
                .chain(iter::repeat_with(|| &mut [][..]))
                .take(thread_count)
        }

        self.clear();

        let threads_len = threads.len();
        self.markers.resize_with(threads_len, default);
        self.mapping.resize_with(threads_len, default);

        assert!(frags.maps.iter().all(|(.., m)| m.is_unique()));

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
                    marker.mark_all(thread, frag_maps);
                    marker.mark_all(roots, frag_maps);
                    marker.mark_all(statics.iter(), frag_maps);
                });
            }
        });

        self.collect_markers(threads_len);

        let mut frag_vec = frags
            .maps
            .drain()
            .map(|(id, frag)| (id, frag, self.marked.remove(&id).unwrap_or_default()))
            .collect::<Vec<_>>();

        thread::scope(|scope| {
            let iter = chunks_mut(&mut frag_vec, threads_len).zip(self.mapping.iter_mut());

            for (maps, mapping) in iter {
                scope.spawn(move || {
                    for (.., frag, marks) in maps {
                        marks.optimize();
                        frag.filter(marks, mapping);
                    }
                });
            }
        });

        self.fold_mapping();

        thread::scope(|scope| {
            let folded_mapping = &self.mapping[0];
            let iter = chunks_mut(&mut frag_vec, threads_len)
                .zip(threads.iter_mut())
                .zip(chunks_mut(&mut frags.roots, threads_len))
                .zip(chunks_mut(&mut frags.cleared, threads_len));

            for (((maps, thread), roots), cleared) in iter {
                scope.spawn(move || {
                    for (.., frag, _) in maps {
                        frag.remap(folded_mapping);
                    }

                    for root in thread {
                        root.remap(folded_mapping);
                    }

                    for root in roots {
                        root.remap(folded_mapping);
                    }

                    for clear in cleared {
                        clear.remap(folded_mapping);
                    }
                });
            }
        });

        frag_vec.into_iter().for_each(|(id, frag, marks)| {
            frags.maps.insert(id, frag);
            self.marked.insert(id, marks);
        });
    }

    fn fold_mapping(&mut self) {
        let (first, rest) = self.mapping.split_first_mut().unwrap();
        let needed_cap = rest.iter().map(|m| m.marked.len()).sum();
        first.marked.reserve(needed_cap);
        for mapping in rest {
            first.marked.extend(mapping.marked.drain());
            first.edges.extend(mapping.edges.drain());
        }
    }
}

#[derive(Default)]
pub struct FragMarks {
    marks: Vec<Vec<FragAddr>>,
    temp: Vec<FragAddr>,
}

impl FragMarks {
    fn append(&mut self, data: &mut Set<FragAddr>, thread_count: usize) {
        self.marks.resize(thread_count, Vec::new());
        for addr in data.drain() {
            let thread = addr.thread() as usize;
            self.marks[thread].push(addr);
        }
    }

    fn optimize(&mut self) {
        for thread in self.marks.iter_mut() {
            self.temp.clear();
            thread.sort_unstable();
            thread.dedup();
            thread
                .group_by(|&a, &b| b.right_after(a))
                .flat_map(|g| [g[0], g[g.len() - 1]])
                .collect_into(&mut self.temp);
            mem::swap(thread, &mut self.temp);
        }
    }

    pub(crate) fn filter_base<
        'a,
        T: Relocated + 'static,
        A: Allocator + 'a,
        S: FnOnce(usize),
        V: Deref<Target = FragVecArc<T, A>>,
    >(
        &mut self,
        base_threads: impl Iterator<Item = (V, S)>,
        mapping: &mut FragRelocMapping,
    ) {
        for (id, (marks, (thread, len_setter))) in self.marks.iter().zip(base_threads).enumerate() {
            let map = |index: usize| DynFragId {
                repr: FragAddr::new(index as u64, id as u8),
                type_id: TypeId::of::<T>(),
            };

            if marks.is_empty() {
                continue;
            }

            assert!(unsafe { FragVecInner::is_unique(thread.0) });
            let (base, current_len) = unsafe {
                let slice = FragVecInner::full_data_mut(thread.0);
                (slice.as_mut_ptr(), slice.len())
            };

            // drop all unmarked items
            let reminder = iter::once((
                marks.last().map_or(FragAddr::new(0, 0), |&r| r),
                FragAddr::new(current_len as u64, 0),
            ));

            for (start, end) in marks[1..]
                .chunks_exact(2)
                .map(|s| (s[0], s[1]))
                .chain(reminder)
            {
                // drop all items between start and end
                let (start_index, ..) = start.parts();
                let (end_index, ..) = end.parts();
                for index in start_index + 1..end_index {
                    unsafe {
                        base.add(index as usize).drop_in_place();
                    }
                }
            }

            // get rid of gaps
            let mut cursor = 0;
            for (start, end) in marks.chunks_exact(2).map(|s| (s[0], s[1])) {
                let (start_index, ..) = start.parts();
                let (end_index, ..) = end.parts();
                let len = (end_index - start_index) as usize + 1;
                unsafe {
                    let source = base.add(start_index as usize);
                    let offset = base.add(cursor);
                    if offset != source {
                        ptr::copy(source, offset, len);
                        let iter = (start_index as usize..=end_index as usize)
                            .map(map)
                            .zip((cursor..cursor + len).map(map));
                        mapping.marked.extend(iter)
                    } else {
                        mapping
                            .edges
                            .insert((TypeId::of::<T>(), id as u8), end_index as usize);
                    }
                    cursor += len;
                }
            }

            len_setter(cursor);
            unsafe {
                ptr::addr_of_mut!((*thread.0.as_ptr()).len).write(cursor);
            }
        }

        self.clear();
    }

    fn clear(&mut self) {
        self.marks.iter_mut().for_each(Vec::clear);
        self.temp.clear();
    }
}

#[derive(Default)]
pub struct RelocatedObjects<'a> {
    maps: Map<TypeId, &'a mut dyn DynFragMap>,
    roots: Vec<&'a mut dyn Relocated>,
    static_roots: Vec<&'a dyn Relocated>,
    cleared: Vec<&'a mut dyn Relocated>,
}

impl<'a> RelocatedObjects<'a> {
    pub fn add(&mut self, frag_map: &'a mut impl DynFragMap) {
        self.maps.insert((*frag_map).item_id(), frag_map);
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
    marked: Map<TypeId, Set<FragAddr>>,
    frontier: Vec<(TypeId, FragAddr)>,
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
            if let Some(frag_map) = relocs.maps.get(&type_id) {
                frag_map.mark(addr, self);
            }
        }
    }

    pub fn mark<T: 'static>(&mut self, frag: FragRef<T>) -> bool {
        self.marked
            .entry(TypeId::of::<T>())
            .or_default()
            .insert(frag.0)
            .then(|| self.frontier.push((TypeId::of::<T>(), frag.0)))
            .is_some()
    }

    pub fn mark_slice<T: 'static>(&mut self, frags: FragSlice<T>) {
        let entry = self.marked.entry(TypeId::of::<T>()).or_default();
        frags
            .0
            .keys()
            .filter(|&f| entry.insert(f))
            .map(|f| (TypeId::of::<T>(), f))
            .collect_into(&mut self.frontier);
    }

    fn clear(&mut self) {
        self.marked.values_mut().for_each(Set::clear);
        self.frontier.clear();
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct DynFragId {
    repr: FragAddr,
    type_id: TypeId,
}

impl DynFragId {
    fn new<T: 'static>(frag: FragRef<T>) -> Self {
        Self {
            repr: frag.0,
            type_id: TypeId::of::<T>(),
        }
    }

    fn downcast<T: 'static>(self) -> Option<FragRef<T>> {
        if self.type_id == TypeId::of::<T>() {
            Some(FragRef::new(self.repr))
        } else {
            None
        }
    }
}

// #[test]
// fn bench_thread_spawn() {

//     let now = Instant::now();
//     let threads = (0..10)
//         .map(|_| thread::spawn(|| ()))
//         .collect::<Vec<_>>();
//     let el = now.elapsed();

//     threads.into_iter().for_each(|t| t.join().unwrap());

//     let le2 = now.elapsed();

//     println!("spawn: {:?}", el);
//     println!("join: {:?}", le2);

//     let now = Instant::now();
//     thread::scope(|scope| {
//         for _ in 0..10 {
//             scope.spawn(|| ());
//         }
//     });
//     println!("scope: {:?}", now.elapsed());
// }

#[macro_export]
macro_rules! derive_relocated {
    (struct $ty:ty { $($field:ident)* }) => {
        impl $crate::Relocated for $ty {
            fn mark(&self, marker: &mut $crate::FragRelocMarker) {
                $(self.$field.mark(marker);)*
            }
            fn remap(&mut self, ctx: &$crate::FragRelocMapping) -> Option<()> {
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
            fn remap(&mut self, ctx: &$crate::FragRelocMapping) -> Option<()> {
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

impl<T: 'static> Relocated for FragRef<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark(*self);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
        *self = ctx.project(*self)?;
        Some(())
    }
}

impl<T: 'static> Relocated for FragSlice<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark_slice(*self);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
        *self = ctx.project_slice(*self);
        Some(())
    }
}

impl<A: Relocated, B: Relocated> Relocated for (A, B) {
    fn mark(&self, marker: &mut FragRelocMarker) {
        self.0.mark(marker);
        self.1.mark(marker);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
        self.0.remap(ctx)?;
        self.1.remap(ctx)
    }
}

impl<K, V, H> Relocated for DashMap<K, V, H>
where
    K: Relocated + Eq + Hash,
    V: Relocated,
    H: Sync + Send + 'static + Clone + BuildHasher,
{
    fn mark(&self, marker: &mut FragRelocMarker) {
        for item in self.iter() {
            item.key().mark(marker);
            item.value().mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
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

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
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

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
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

                fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
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
                fn remap(&mut self, _: &FragRelocMapping) -> Option<()> { Some(()) }
            }
        )*
    };
}

impl_blanc_relocated! {
    bool
}

impl<T: Relocated> Relocated for Arc<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        (**self).mark(marker);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) -> Option<()> {
        let s = Arc::get_mut(self).expect("arc was expected to be unique for remapping");
        s.remap(ctx)
    }
}
