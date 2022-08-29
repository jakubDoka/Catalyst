#![feature(string_extend_from_within)]
#![feature(int_log)]
#![feature(result_into_ok_or_err)]
#![feature(let_else)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]

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

#[macro_export]
macro_rules! gen_constant_groups {
    ($($name:ident = [$($elem:ident)*];)*) => {
        $(
            pub const $name: &'static [VRef<Self>] = &[$(Self::$elem),*];
        )*
    };
}

#[macro_export]
macro_rules! gen_increasing_constants {
    ($($ident:ident)*) => {
        gen_increasing_constants!((0) $($ident)*);
        gen_constant_groups!(ALL = [$($ident)*];);
    };

    (($prev:expr) $current:ident $($next:ident $($others:ident)*)?) => {
        pub const $current: VRef<Self> = unsafe { VRef::new($prev) };
        $(
            gen_increasing_constants!((Self::$current.index() + 1) $next $($others)*);
        )?
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
        [$(InternedSegment::from($item)),*]
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

/// Set of virtual pointers. Compared to [`Vec`]<[`bool`]> it uses 8x less memory.
mod bit_set;
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
/// Similar to ordered map but some indexes may not have mapping.
mod partial_ordered_map;
/// Bump map with ability to reallocate slices.
mod pool_bump_map;
/// Vector abstraction that allows reusing allocations while preserving other values.
mod pool_map;
/// Trait representing virtual pointer.
mod primitives;
/// Storage that can map additional info for existing map.
mod shadow_map;
/// Similar to shadow map, but lot more memory efficient when storing big structures
/// that are sparsely populated.
mod sparse_map;

pub use bit_set::BitSet;
pub use bump_alloc::*;
pub use bump_map::{BumpMap, CacheBumpMap, Reserved};
pub use clear::Clear;
pub use frames::Frames;
pub use interner::{ident_join, Ident, InternedSegment, Interner};
pub use invalid::{Invalid, Maybe};
pub use map::{IdentPair, Map, SpecialHash};
pub use ordered_map::OrderedMap;
pub use partial_ordered_map::PartialOrderedMap;
pub use pool_bump_map::{CachedPoolBumpMap, PoolBumpMap};
pub use pool_map::PoolMap;
pub use primitives::{VRef, VRefDefault, VSlice};
pub use shadow_map::ShadowMap;
pub use sparse_map::SparseMap;
