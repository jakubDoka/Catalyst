use std::{
    marker::PhantomData,
    ops::{Index, IndexMut, Range},
};

use cranelift_entity::{packed_option::ReservedValue, EntityRef};

use crate::{FramedStack, BitSerde};

pub struct FramedStackMap<E: EntityRef, T> {
    lists: StackMap<E, T>,
    stack: FramedStack<T>,
}

impl<E: EntityRef + ReservedValue, T: Clone> FramedStackMap<E, T> {
    pub fn new() -> Self {
        Self {
            lists: StackMap::new(),
            stack: FramedStack::new(),
        }
    }

    pub fn len_of(&self, list: E) -> usize {
        self.lists.len_of(list)
    }

    pub fn get(&self, list: E) -> &[T] {
        self.lists.get(list)
    }

    pub fn push(&mut self, slice: &[T]) -> E {
        self.lists.push(slice)
    }

    pub fn push_iter(&mut self, iter: impl Iterator<Item = T>) -> E {
        self.lists.push_iter(iter)
    }

    pub fn mark_frame(&mut self) {
        self.stack.mark_frame();
    }

    pub fn push_one(&mut self, ty: T) {
        self.stack.push(ty);
    }

    pub fn top(&self) -> &[T] {
        self.stack.top_frame()
    }

    pub fn top_mut(&mut self) -> &mut [T] {
        self.stack.top_frame_mut()
    }

    pub fn pop_frame(&mut self) -> E {
        let list = self.lists.push(self.stack.top_frame());
        self.stack.pop_frame();
        list
    }

    pub fn discard(&mut self) {
        self.stack.pop_frame();
    }

    pub fn deep_clone(&mut self, target: E) -> E {
        self.lists.deep_clone(target)
    }

    pub fn get_mut(&mut self, list: E) -> &mut [T] {
        self.lists.get_mut(list)
    }

    pub fn clear(&mut self) {
        self.lists.clear();
    }

    pub fn push_from_within(&mut self, list: E) {
        self.stack.extend(self.lists.get(list));
    }

    pub fn join(&mut self, a: E, b: E) -> E {
        self.lists.join(a, b)
    }

    pub fn join_low(&mut self, a: E, b: E, reuse: bool) -> E {
        self.lists.join_low(a, b, reuse)
    }
}

#[derive(Clone, Debug)]
pub struct StackMap<E: EntityRef, T, S: EntityRef = Unused> {
    data: Vec<T>,
    indices: Vec<u32>,
    _ph: PhantomData<(E, S)>,
}

impl<E: EntityRef, T, S: EntityRef> StackMap<E, T, S> {
    pub fn new() -> Self {
        StackMap {
            data: Vec::new(),
            indices: vec![0],
            _ph: PhantomData,
        }
    }

    pub fn deep_clone(&mut self, id: E) -> E
    where
        T: Clone,
        E: ReservedValue,
    {
        let from = self.range_of(id);
        self.data.extend_from_within(from);
        self.close_frame()
    }

    pub fn alloc(&mut self, size: usize, init: T) -> E
    where
        T: Clone,
        E: ReservedValue,
    {
        let index = self.data.len();
        self.data.resize(index + size, init);

        self.close_frame()
    }

    pub fn push_iter(&mut self, iter: impl IntoIterator<Item = T>) -> E
    where
        E: ReservedValue,
    {
        self.data.extend(iter);
        self.close_frame()
    }

    pub fn push(&mut self, value: &[T]) -> E
    where
        T: Clone,
        E: ReservedValue,
    {
        self.data.extend_from_slice(value);
        self.close_frame()
    }

    #[inline]
    pub fn get(&self, id: E) -> &[T] {
        &self.data[self.range_of(id)]
    }

    pub fn len(&self) -> usize {
        self.indices.len() - 1
    }

    pub fn push_to(&mut self, to: &mut E, elem: T)
    where
        E: ReservedValue,
        T: Clone,
    {
        let range = self.range_of(*to);
        self.data.extend_from_within(range);
        self.push_one(elem);
        *to = self.close_frame();
    }

    pub fn join(&mut self, a: E, b: E) -> E
    where
        E: ReservedValue,
        T: Clone,
    {
        self.join_low(a, b, true)
    }

    pub fn join_low(&mut self, a: E, b: E, reuse: bool) -> E
    where
        E: ReservedValue,
        T: Clone,
    {
        let (a_s, b_s) = (self.range_of(a), self.range_of(b));

        if a_s.is_empty() && reuse {
            return b;
        }

        if b_s.is_empty() && reuse {
            return a;
        }

        self.data.extend_from_within(a_s);
        self.data.extend_from_within(b_s);

        self.close_frame()
    }

    pub fn slice_keys(&self, id: E) -> impl Iterator<Item = S> + Clone {
        self.range_of(id).map(S::new)
    }

    pub fn len_of(&self, id: E) -> usize {
        self.range_of(id).len()
    }

    pub fn get_mut(&mut self, id: E) -> &mut [T] {
        let range = self.range_of(id);
        &mut self.data[range]
    }

    pub fn range_of(&self, id: E) -> Range<usize> {
        match (
            self.indices.get(id.index()),
            self.indices.get(id.index() + 1),
        ) {
            (Some(start), Some(end)) => *start as usize..*end as usize,
            _ => 0..0,
        }
    }

    pub fn start_index_of(&self, id: E) -> Option<usize> {
        self.indices.get(id.index()).map(|&i| i as usize)
    }

    pub fn get_iter(&self, id: E) -> impl Iterator<Item = (S, &T)> + Clone {
        self.slice_keys(id).map(|k| (k, &self[k]))
    }

    pub fn push_one(&mut self, value: T) -> S {
        let id = self.data.len();
        self.data.push(value);
        S::new(id)
    }

    pub fn close_frame(&mut self) -> E
    where
        E: ReservedValue,
    {
        if self.data.len() == self.indices[self.indices.len() - 1] as usize {
            return E::reserved_value();
        }

        let id = self.indices.len() - 1;
        self.indices.push(self.data.len() as u32);
        E::new(id)
    }

    pub fn top(&self) -> &[T] {
        let id = self.indices.len() - 1;
        &self.data[self.indices[id] as usize..]
    }

    pub fn top_mut(&mut self) -> &mut [T] {
        let id = self.indices.len() - 1;
        &mut self.data[self.indices[id] as usize..]
    }

    pub fn discard(&mut self) {
        let id = self.indices.len() - 1;
        self.data.truncate(self.indices[id] as usize);
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.indices.truncate(1);
    }

    pub fn drain(&mut self) -> impl Iterator<Item = (E, T)> + '_ {
        self.indices.clear();
        self.data.drain(..).enumerate().map(move |(i, v)| {
            let id = E::new(i);
            (id, v)
        })
    }

    pub fn values(&self) -> impl Iterator<Item = &[T]> + Clone {
        self.indices
            .windows(2)
            .map(|window| &self.data[window[0] as usize..window[1] as usize])
    }

    pub fn key_of(&self, id: E, index: usize) -> Option<S> {
        self.start_index_of(id).map(|i| S::new(i + index))
    }
}

impl<E: EntityRef, T: BitSerde, S: EntityRef> BitSerde for StackMap<E, T, S> {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.indices.write(buffer);
        self.data.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        Ok(StackMap {
            indices: Vec::read(cursor, buffer)?,
            data: Vec::read(cursor, buffer)?,
            _ph: PhantomData,
        })
    }
}

impl<E: EntityRef, T, S: EntityRef> Default for StackMap<E, T, S> {
    fn default() -> Self {
        StackMap::new()
    }
}

impl<E: EntityRef, T, S: EntityRef> Index<S> for StackMap<E, T, S> {
    type Output = T;

    fn index(&self, id: S) -> &Self::Output {
        &self.data[id.index()]
    }
}

impl<E: EntityRef, T, S: EntityRef> IndexMut<S> for StackMap<E, T, S> {
    fn index_mut(&mut self, id: S) -> &mut Self::Output {
        &mut self.data[id.index()]
    }
}

crate::gen_entity!(Unused);
