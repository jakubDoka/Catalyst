use std::{marker::PhantomData, ops::Index};

use cranelift_entity::{packed_option::ReservedValue, EntityRef};

use crate::FramedStack;

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

    pub fn len(&self, list: E) -> usize {
        self.lists.len(list)
    }

    pub fn get(&self, list: E) -> &[T] {
        self.lists.get(list)
    }

    pub fn push(&mut self, slice: &[T]) -> E {
        self.lists.push(slice)
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

    pub fn clear(&mut self) {
        self.lists.clear();
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
        match (
            self.indices.get(id.index()),
            self.indices.get(id.index() + 1),
        ) {
            (Some(start), Some(end)) => &self.data[*start as usize..*end as usize],
            _ => &[],
        }
    }

    pub fn len(&self, id: E) -> usize {
        match (
            self.indices.get(id.index()),
            self.indices.get(id.index() + 1),
        ) {
            (Some(start), Some(end)) => *end as usize - *start as usize,
            _ => 0,
        }
    }

    pub fn get_mut(&mut self, id: E) -> &mut [T] {
        match (
            self.indices.get(id.index()),
            self.indices.get(id.index() + 1),
        ) {
            (Some(start), Some(end)) => &mut self.data[*start as usize..*end as usize],
            _ => &mut [],
        }
    }

    pub fn start_index_of(&self, id: E) -> Option<usize> {
        self.indices.get(id.index()).map(|&i| i as usize)
    }

    pub fn get_iter(&self, id: E) -> impl Iterator<Item = (S, &T)> + Clone {
        self.get(id)
            .iter()
            .enumerate()
            .map(move |(i, v)| (S::new(i + id.index()), v))
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
}

impl<E: EntityRef, T, S: EntityRef> Index<S> for StackMap<E, T, S> {
    type Output = T;

    fn index(&self, id: S) -> &Self::Output {
        &self.data[id.index()]
    }
}

crate::gen_entity!(Unused);
