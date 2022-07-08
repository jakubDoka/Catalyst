#![feature(string_extend_from_within)]
#![feature(int_log)]

//! Crate contains all primitives for storing data in most efficient way, used by compiler.
//! Some concepts are identical to cranelifts way of handling things but they are rewritten 
//! here for more convenience.
//! 
//! Core component of storage system is [`VPtr`] which is a trait that defines typed virtual 
//! pointer, usually pointing to item inside the vector. All storages use this as an access 
//! point and store it in its own structures. Advantage over just usize is the size and type 
//! safety. Each container should have distinct [`VPtr`] unless it is a storage plugin.
//! 
//! Another important part of API is [`Maybe`]. This value wrapper can optionally store a 
//! value without any extra memory for flags. Its based of [`Invalid`] trait which determines
//! whether value is invalid thus the [`Maybe`] holds nothing.


/// Macro generates type with [`VPtr`] implemented. The pointers usually don't differ in 
/// implementation, they just need to be distinct.
/// 
/// # Examples
/// ```
/// // supports bulk declaration 
/// storage::gen_v_ptr!(Something SomethingElse);
/// ```
#[macro_export]
macro_rules! gen_v_ptr {
    ($($ty:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            pub struct $ty(u32);
        
            impl $crate::v_ptr::VPtr for $ty {
                fn new(index: usize) -> Self {
                    assert!(index as u32 != u32::MAX);
                    $ty(index as u32)
                }
                
                fn index(&self) -> usize {
                    self.0 as usize
                }
            }

            impl $crate::invalid::Invalid for $ty {
                unsafe fn invalid() -> Self {
                    $ty(u32::MAX)
                }

                fn is_invalid(&self) -> bool {
                    self.0 == u32::MAX
                }
            }

            impl $crate::serde::Serialize for $ty {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: $crate::serde::Serializer,
                {
                    self.0.serialize(serializer)
                }
            }

            impl<'a> $crate::serde::Deserialize<'a> for $ty {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: $crate::serde::Deserializer<'a>,
                {
                    u32::deserialize(deserializer).map($ty)
                }
            }
        )*
    };
}

/// Macro makes it easy to construct '&[[`InternedSegment`]]'.
/// 
/// # Examples
/// ```
/// use storage::*;
/// 
/// let mut interner = Interner::new();
/// 
/// let a = interner.intern_str("a");
/// 
/// let usage: &[InternedSegment] = ident!("h", 10, a);
/// let result: &[InternedSegment] = &["h".into(), 10.into(), a.into()];
/// 
/// assert_eq!(usage, result);
/// ```
#[macro_export]
macro_rules! ident {
    ($($item:expr),* $(,)?) => {
        &[$($item.into()),*]
    };
}

pub extern crate serde;

pub mod interner;
pub mod v_ptr;
pub mod invalid;
pub mod sparse_map;
pub mod pool_map;
pub mod v_ptr_set;
pub mod bump_map;
pub mod pool_bump_map;
pub mod frames;

pub use interner::{Interner, InternedSegment};
pub use v_ptr::{VPtr};
pub use invalid::{Invalid, Maybe};
pub use sparse_map::{SparseMap};
pub use pool_map::{PoolMap};
pub use v_ptr_set::{VPtrSet};
pub use bump_map::{BumpMap, CacheBumpMap};
pub use pool_bump_map::{PoolBumpMap};
pub use frames::{Frames};