#![feature(auto_traits)]
#![feature(negative_impls)]

pub extern crate cranelift_entity;

pub mod logic;
pub mod map;
pub mod source_info;
pub mod token;

pub use logic::*;
pub use map::*;
pub use source_info::*;
pub use token::Token;

#[macro_export]
macro_rules! gen_entity {
    ($($name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct $name(pub u32);

            $crate::cranelift_entity::entity_impl!($name);

            impl Default for $name {
                fn default() -> Self {
                    $crate::cranelift_entity::packed_option::ReservedValue::reserved_value()
                }
            }


            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(f, "{}{}", stringify!($name).chars().next().unwrap().to_lowercase(), self.0)
                }
            }

            impl $crate::map::IDFilter for $name {}
        )*
    };
}

use cranelift_entity::{packed_option::ReservedValue, EntityList, EntityRef, ListPool};

pub trait ListPoolExt<T: EntityRef + ReservedValue> {
    fn list(&mut self, slice: &[T]) -> EntityList<T>;
    fn view(&self, list: EntityList<T>) -> &[T];
}

impl<T: EntityRef + ReservedValue> ListPoolExt<T> for ListPool<T> {
    fn list(&mut self, slice: &[T]) -> EntityList<T> {
        EntityList::from_slice(slice, self)
    }

    fn view(&self, list: EntityList<T>) -> &[T] {
        list.as_slice(self)
    }
}
