#![feature(bool_to_option)]
#![feature(auto_traits)]
#![feature(negative_impls)]

pub mod map;
pub mod stack_map;
pub mod framed_stack;
pub mod ansi_consts;
pub mod tree;
pub mod pool_map;
pub mod sparse_map;
pub mod bit_serde;

pub use framed_stack::FramedStack;
pub use stack_map::{StackMap, FramedStackMap};
pub use map::{Map, ID, SecondaryMap, Set};
pub use ansi_consts::{ERR, WARNING, INFO, SUCCESS, END};
pub use cranelift_entity::{PrimaryMap, EntityRef, EntitySet, packed_option::{ReservedValue, PackedOption}};
pub use bitflags::bitflags;
pub use tree::*;
pub use pool_map::PoolMap;
pub use bit_serde::BitSerde;

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