#![feature(type_name_of_val)]
#![feature(if_let_guard)]

use std::{any::{Any, TypeId}, collections::HashMap};

use ansi_term::Color;

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
        self.errors.get(&TypeId::of::<T>())
            .map(|storage| storage.downcast_ref::<Vec<T>>().unwrap().iter())
    }

    pub fn clear(&mut self) {
        self.errors.clear();
    }
}

/// Scope struct for commonly used colors
pub struct Palette;

impl Palette {
    pub fn info() -> Color {
        Color::RGB(138, 226, 255)
    }

    pub fn error() -> Color {
        Color::RGB(255, 92, 92)
    }

    pub fn warning() -> Color {
        Color::RGB(255, 255, 138)
    }

    pub fn success() -> Color {
        Color::RGB(0, 204, 0)
    }
}

#[macro_export]
macro_rules! write_styled {
    ($target:expr, $style:expr, $str:literal, $($args:expr),*) => {
        {
            let style: ansi_term::Style = $style.into();
            write!($target, "{}", style.prefix()).unwrap();
            write!($target, $str, $($args),*).unwrap();
            write!($target, "{}", style.suffix()).unwrap();
        }
    };
}
