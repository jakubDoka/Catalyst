use cranelift_entity::{EntityRef, packed_option::ReservedValue};

use crate::StackMap;

pub struct FramedStack<T> {
    data: Vec<T>,
    frames: Vec<u32>,
}

impl<T> FramedStack<T> {
    pub fn new() -> Self {
        FramedStack {
            data: Vec::new(),
            frames: vec![0],
        }
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
    }

    pub fn push_default(&mut self)
    where
        T: Default,
    {
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
        self.data
            .truncate(self.frames.pop().unwrap().clone() as usize);
    }

    pub fn save_and_pop_frame<E: EntityRef + ReservedValue, S: EntityRef>(
        &mut self,
        stack_map: &mut StackMap<E, T, S>,
    ) -> E
    where
        T: Clone,
    {
        let id = stack_map.push(self.top_frame());
        self.pop_frame();
        id
    }
}
