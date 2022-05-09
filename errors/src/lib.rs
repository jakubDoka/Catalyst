#![feature(type_name_of_val)]
#![feature(if_let_guard)]

use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

/// Shorthand for common result type
pub type Result<T = ()> = std::result::Result<T, ()>;

pub struct Diagnostics {
    pub errors: HashMap<TypeId, Box<dyn Any>>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            errors: HashMap::new(),
        }
    }

    pub fn push<T: Any + 'static>(&mut self, error: T) {
        if let Some(storage) = self.errors.get_mut(&error.type_id()) {
            storage.downcast_mut::<Vec<T>>().unwrap().push(error);
        } else {
            self.errors.insert(error.type_id(), Box::new(vec![error]));
        }
    }

    pub fn iter<T: Any + 'static>(&self) -> Option<impl Iterator<Item = &T>> {
        self.errors
            .get(&TypeId::of::<T>())
            .map(|storage| storage.downcast_ref::<Vec<T>>().unwrap().iter())
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn clear(&mut self) {
        self.errors.clear();
    }
}
