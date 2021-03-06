#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(vec_into_raw_parts)]

pub mod ansi_consts;
pub mod bit_serde;
pub mod framed_stack;
pub mod instances;
pub mod map;
pub mod pool_map;
pub mod sparse_map;
pub mod stack_map;
pub mod tree;
pub mod vec_pool;

pub use ansi_consts::{END, ERR, INFO, SUCCESS, WARNING, WEAK};
pub use bit_serde::BitSerde;
pub use bitflags::bitflags;
pub use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityList, EntityRef, EntitySet, ListPool, PrimaryMap,
};
pub use framed_stack::FramedStack;
pub use instances::{HasMeta, MetaMap, MetaRef};
pub use map::{Map, SecondaryMap, Set, ID};
pub use pool_map::PoolMap;
pub use sparse_map::SparseMap;
pub use stack_map::{FramedStackMap, StackMap};
pub use tree::{CycleDetectResources, GenericGraph, TreeStorage};
pub use vec_pool::{PoolVec, VecPool};

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
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
