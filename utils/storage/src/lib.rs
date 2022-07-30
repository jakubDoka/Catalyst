#![feature(string_extend_from_within)]
#![feature(int_log)]
#![feature(result_into_ok_or_err)]
#![feature(let_else)]

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

#[macro_export]
macro_rules! impl_flag_and_bool {
    ($ty:ty) => {
        impl std::ops::BitAnd<bool> for $ty {
            type Output = Self;

            fn bitand(self, rhs: bool) -> Self {
                if rhs {
                    self
                } else {
                    Self::empty()
                }
            }
        }
    };
}

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

            impl $crate::VPtr for $ty {
                fn new(index: usize) -> Self {
                    assert!(index as u32 != u32::MAX);
                    $ty(index as u32)
                }

                fn index(&self) -> usize {
                    self.0 as usize
                }
            }

            impl $crate::Invalid for $ty {
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

            impl std::fmt::Display for $ty {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(f, "{}{}", stringify!($ty).to_ascii_lowercase(), self.0)
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

#[macro_export]
macro_rules! bitflags {
    (
        struct $name:ident: $repr:ty {
            $($first:ident $($item:ident)*)?
        }
    ) => {
        $crate::bitflags::bitflags! {
            #[derive(Default)]
            pub struct $name: $repr {

            }
        }

        impl $name {
            $(
                bitflags!(__fields__ (1, $name), $first $($item)*);
            )?
        }

        impl_flag_and_bool!($name);
    };

    (__fields__ ($prev:expr, $repr:ident), $current:ident $($next:ident $($other:ident)*)?) => {
        pub const $current: $repr = $repr { bits: $prev };
        $( bitflags!(__fields__ (Self::$current.bits << 1, $repr), $next $($other)*); )?
    }
}

pub extern crate bitflags;
pub extern crate serde;

/// Bump allocator for fixed size slices.
mod bump_map;
/// Trait representing reusable object.
mod clear;
/// Homogenous stack with frame markers.
mod frames;
/// String to uid mapping.
mod interner;
/// Trait representing invalid value and wrapper [`Maybe`] struct for 
/// representing optional values without memory costs.
mod invalid;
/// HashMap wrapper to work with [`Ident`].
mod map;
/// Map with addressable values. (by [`VPtr`])
mod ordered_map;
/// Bump map with ability to reallocate slices.
mod pool_bump_map;
/// Vector abstraction that allows reusing allocations while preserving other values.
mod pool_map;
/// Storage that can map additional info for existing map.
mod shadow_map;
/// Similar to shadow map, but lot more memory efficient when storing big structures
/// that are sparsely populated.
mod sparse_map;
/// Trait representing virtual pointer.
mod v_ptr;
/// Set of virtual pointers. Compared to [`Vec`]<[`bool`]> it uses 8x less memory.
mod v_ptr_set;

pub use bump_map::{BumpMap, CacheBumpMap};
pub use clear::Clear;
pub use frames::Frames;
pub use interner::{Ident, InternedSegment, Interner};
pub use invalid::{Invalid, Maybe};
pub use map::Map;
pub use ordered_map::OrderedMap;
pub use pool_bump_map::{CachedPoolBumpMap, PoolBumpMap};
pub use pool_map::PoolMap;
pub use shadow_map::ShadowMap;
pub use sparse_map::SparseMap;
pub use v_ptr::VPtr;
pub use v_ptr_set::VPtrSet;
