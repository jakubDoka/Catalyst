use std::{marker::PhantomData, ops::Index};

use cranelift_entity::EntityRef;

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

    pub fn push(&mut self, value: &[T]) -> E where T: Clone {
        self.data.extend_from_slice(value);
        self.close_frame()
    }

    pub fn get(&self, id: E) -> &[T] {
        match (self.indices.get(id.index()), self.indices.get(id.index() + 1)) {
            (Some(start), Some(end)) => &self.data[*start as usize..*end as usize],
            _ => &[],
        }
    }

    pub fn push_one(&mut self, value: T) -> S {
        let id = self.data.len();
        self.data.push(value);
        S::new(id)
    }

    pub fn close_frame(&mut self) -> E {
        let id = self.indices.len() - 1;
        self.indices.push(self.data.len() as u32);
        E::new(id)
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

pub struct Stack<T> {
    data: Vec<T>,
    frames: Vec<u32>,   
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack {
            data: Vec::new(),
            frames: vec![0],
        }
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
    }

    pub fn push_default(&mut self) where T: Default {
        self.data.push(T::default());
    }

    pub fn mark_frame(&mut self) {
        self.frames.push(self.data.len() as u32);
    }

    pub fn top_frame(&self) -> &[T] {
        &self.data[self.frames.last().unwrap().clone() as usize..]
    }

    pub fn pop(&mut self) -> T {
        self.data.pop().unwrap()
    }

    pub fn pop_frame(&mut self) {
        self.data.truncate(self.frames.pop().unwrap().clone() as usize);
    }

    pub fn save_and_pop_frame<E: EntityRef>(&mut self, stack_map: &mut StackMap<E, T>) -> E where T: Clone {
        let id = stack_map.push(self.top_frame());
        self.pop_frame();
        id
    }
}