use std::{
    alloc::Allocator,
    any::{Any, TypeId},
    default::default,
    hash::{BuildHasher, Hash},
    mem, ptr,
    sync::Arc,
    thread,
};

use dashmap::DashMap;
use smallvec::SmallVec;

use crate::*;

use super::{FragVecArc, FragVecInner};

pub trait Relocated: 'static + Send + Sync {
    fn mark(&self, marker: &mut FragRelocMarker);
    fn remap(&mut self, ctx: &FragRelocMapping);
}

#[derive(Default)]
pub struct FragRelocMapping {
    marked: Map<DynFragId, DynFragId>,
}

impl FragRelocMapping {
    fn project_base<T: Relocated, A: Allocator>(&self, base: &mut FragBase<T, A>) {
        base.threads
            .iter_mut()
            .flat_map(|f| f.unique_data())
            .for_each(|item| item.remap(self));
    }

    pub fn project<T: 'static>(&self, frag: FragRef<T>) -> FragRef<T> {
        self.marked
            .get(&DynFragId::new(frag))
            .and_then(|id| id.downcast())
            .unwrap_or(frag) // unchanged frags are not in map
    }

    pub fn project_slice<T: 'static>(&self, slice: FragSlice<T>) -> FragSlice<T> {
        let Some(start) = slice.keys().next() else {
            return FragSlice::empty();
        };

        let (index, thread) = self.project(start).0.parts();

        FragSlice::new(FragSliceAddr::new(index, thread, slice.len() as u16))
    }
}

pub trait DynFragMap: Send + Sync {
    fn mark(&self, addr: FragAddr, marker: &mut FragRelocMarker);
    fn remap(&mut self, ctx: &FragRelocMapping);
    fn filter(&mut self, marks: &mut FragMarks, mapping: &mut FragRelocMapping);
}

impl<T: Relocated, A: Allocator + Send + Sync> DynFragMap for FragBase<T, A> {
    fn mark(&self, addr: FragAddr, marker: &mut FragRelocMarker) {
        let addr = FragRef::<T>::new(addr);
        self[addr].mark(marker);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        ctx.project_base(self);
    }

    fn filter(&mut self, marks: &mut FragMarks, mapping: &mut FragRelocMapping) {
        marks.filter_base(
            self.threads.iter_mut().map(|t| {
                (&t.inner, |len| {
                    t.len = len;
                    t.frozen = len;
                })
            }),
            mapping,
        );
    }
}

#[derive(Default)]
pub struct FragRelocator {
    marked: Map<TypeId, FragMarks>,
    markers: Vec<FragRelocMarker>,
    mapping: Vec<FragRelocMapping>,
}

impl FragRelocator {
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
        self.markers.resize_with(threads.len(), default);
        self.mapping.resize_with(threads.len(), default);

        thread::scope(|scope| {
            let frag_maps = &*frags;
            for (marker, roots) in self.markers.iter_mut().zip(threads.iter_mut()) {
                scope.spawn(move || {
                    marker.mark_all(roots.drain(..), frag_maps);
                });
            }
        });

        self.collect_markers(threads.len());

        let mut frag_vec = frags
            .maps
            .drain()
            .map(|(id, frag)| (id, frag, self.marked.remove(&id).unwrap_or_default()))
            .collect::<Vec<_>>();
        let chunk_calc = |len| len / threads.len() + (len % threads.len() != 0) as usize;
        let frag_chunk_size = chunk_calc(frag_vec.len());

        thread::scope(|scope| {
            for (chunk, mapping) in frag_vec
                .chunks_mut(frag_chunk_size)
                .zip(self.mapping.iter_mut())
            {
                scope.spawn(move || {
                    for (.., frag, marks) in chunk {
                        marks.optimize();
                        frag.filter(marks, mapping);
                    }
                });
            }
        });

        self.fold_mapping();

        let root_chunk_size = chunk_calc(frags.roots.len());

        thread::scope(|scope| {
            let folded_mapping = &self.mapping[0];
            let mut root_chunks = frags.roots.chunks_mut(root_chunk_size);
            for chunk in frag_vec.chunks_mut(frag_chunk_size) {
                let root_chunk = root_chunks.next();
                scope.spawn(move || {
                    for (.., frag, _) in chunk {
                        frag.remap(folded_mapping);
                    }

                    if let Some(chunk) = root_chunk {
                        for root in chunk {
                            root.remap(folded_mapping);
                        }
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
        self.marks
            .iter_mut()
            .for_each(|thread| thread.sort_unstable());

        for thread in self.marks.iter_mut() {
            self.temp.clear();
            thread
                .group_by(|&a, &b| b.right_after(a))
                .flat_map(|g| [g[0], g[g.len() - 1]])
                .collect_into(&mut self.temp);
            mem::swap(thread, &mut self.temp);
        }
    }

    fn filter_base<'a, T: Relocated, A: Allocator + 'a, S: FnOnce(usize)>(
        &mut self,
        base_threads: impl Iterator<Item = (&'a FragVecArc<T, A>, S)>,
        mapping: &mut FragRelocMapping,
    ) {
        for (id, (marks, (thread, len_setter))) in self.marks.iter().zip(base_threads).enumerate() {
            let map = |index: usize| DynFragId {
                repr: FragAddr::new(index as u64, id as u8),
                type_id: TypeId::of::<T>(),
            };

            assert!(unsafe { FragVecInner::is_unique(thread.0) });
            let base = unsafe { FragVecInner::full_data_mut(thread.0) }.as_mut_ptr();

            // drop all unmarked items
            for (start, end) in marks[1..].chunks_exact(2).map(|s| (s[0], s[1])) {
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
                    let source = FragVecInner::get_item(thread.0, start_index as usize).as_ptr();
                    let offset = base.add(cursor);
                    if offset != source {
                        ptr::copy(source, offset, len);
                        let iter = (start_index as usize..=end_index as usize)
                            .map(map)
                            .zip((cursor..cursor + len).map(map));
                        mapping.marked.extend(iter)
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
}

impl<'a> RelocatedObjects<'a> {
    pub fn add(&mut self, frag_map: &'a mut (impl DynFragMap + Any)) {
        self.maps.insert((*frag_map).type_id(), frag_map);
    }

    pub fn add_root(&mut self, root: &'a mut dyn Relocated) {
        self.roots.push(root);
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
    pub fn mark_all<T: Relocated>(
        &mut self,
        roots: impl IntoIterator<Item = T>,
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
        for frag in frags.keys() {
            if entry.insert(frag.0) {
                self.frontier.push((TypeId::of::<T>(), frag.0));
            }
        }
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
            fn remap(&mut self, ctx: &$crate::FragRelocMapping) {
                $(self.$field.remap(ctx);)*
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
            fn remap(&mut self, ctx: &$crate::FragRelocMapping) {
                use $ty::*;
                match self {
                    $($pat => {
                        $($field.remap(ctx);)*
                    },)*
                }
            }
        }
    };
}

impl<T: 'static> Relocated for FragRef<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark(*self);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        *self = ctx.project(*self);
    }
}

impl<T: 'static> Relocated for FragSlice<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark_slice(*self);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        *self = ctx.project_slice(*self);
    }
}

impl<A: Relocated, B: Relocated> Relocated for (A, B) {
    fn mark(&self, marker: &mut FragRelocMarker) {
        self.0.mark(marker);
        self.1.mark(marker);
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        self.0.remap(ctx);
        self.1.remap(ctx);
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

    fn remap(&mut self, ctx: &FragRelocMapping) {
        let new_map = DashMap::with_capacity_and_hasher(self.len(), self.hasher().clone());
        for (mut key, mut value) in mem::replace(self, new_map).into_iter() {
            key.remap(ctx);
            value.remap(ctx);
            self.insert(key, value);
        }
    }
}

impl<T: Relocated> Relocated for [T] {
    fn mark(&self, marker: &mut FragRelocMarker) {
        for item in self {
            item.mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        for item in self {
            item.remap(ctx);
        }
    }
}

impl<T: Relocated> Relocated for Option<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        if let Some(item) = self {
            item.mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragRelocMapping) {
        if let Some(item) = self {
            item.remap(ctx);
        }
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

                fn remap(&mut self, ctx: &FragRelocMapping) {
                    (**self).remap(ctx);
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
                fn remap(&mut self, _: &FragRelocMapping) {}
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

    fn remap(&mut self, ctx: &FragRelocMapping) {
        let s = Arc::get_mut(self).expect("arc was expected to be unique for remapping");
        s.remap(ctx);
    }
}
