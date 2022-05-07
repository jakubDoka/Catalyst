#![feature(bool_to_option)]

pub mod map;
pub mod stack_map;
pub mod framed_stack;
pub mod ansi_consts;
pub mod tree;

pub use framed_stack::*;
pub use stack_map::*;
pub use map::{*, SecondaryMap};
pub use ansi_consts::*;
pub use cranelift_entity::{*, packed_option::*};
pub use bitflags::*;
pub use tree::*;

pub extern crate cranelift_entity;

#[macro_export]
macro_rules! impl_bool_bit_and {
    ($flags:ty) => {
        impl std::ops::BitAnd<bool> for $flags {
            type Output = Self;

            fn bitand(self, rhs: bool) -> Self {
                Self {
                    bits: self.bits & (!rhs as u32).wrapping_add(u32::MAX),
                }
            }
        }
    };
}

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