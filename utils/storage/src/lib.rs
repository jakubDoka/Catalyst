#![feature(string_extend_from_within)]

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
/// use serde::{Serialize, Deserialize};
/// 
/// // supports bulk declaration 
/// storage::gen_v_ptr!(Something SomethingElse);
/// ```
#[macro_export]
macro_rules! gen_v_ptr {
    ($($ty:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
            pub struct $ty(u32);
        
            impl $crate::v_ptr::VPtr for $ty {
                fn new(index: usize) -> Self {
                    $ty(index as u32)
                }
                
                fn index(&self) -> usize {
                    self.0 as usize
                }
            }

            impl $crate::invalid::Invalid for $ty {
                fn invalid() -> Self {
                    $ty(u32::MAX)
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

pub mod interner;
pub mod v_ptr;
pub mod invalid;
pub mod sparse_map;

pub use interner::{Interner, InternedSegment};
pub use v_ptr::{VPtr};
pub use invalid::{Invalid, Maybe};
pub use sparse_map::{};