#![feature(string_extend_from_within)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(const_trait_impl)]
#![feature(rustc_attrs)]
#![feature(new_uninit)]
#![feature(const_discriminant)]
#![feature(atomic_bool_fetch_not)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(macro_metavar_expr)]
#![feature(allocator_api)]
#![feature(let_chains)]

#[macro_export]
macro_rules! function_pointer {
    ($($name:ident$(($($arg_name:ident: $arg:ty),*))? $(-> $ret:ty)?,)+) => {
        $(
            #[repr(C)]
            #[derive(Copy, Clone)]
            pub struct $name<'a>(&'a u8);

            impl $name<'_> {
                pub fn call(self, $($($arg_name: $arg,)*)?) $(-> $ret)? {
                    unsafe {
                        let ptr = std::mem::transmute::<_, extern "C" fn($($($arg),*)?) $(-> $ret)?>(self.0);
                        ptr($($($arg_name),*)?)
                    }
                }
            }

            impl<'a> FunctionPointer<'a> for $name<'a> {
                fn new(ptr: &'a u8) -> Self {
                    Self(ptr)
                }
            }
        )+
    };

    (@ret) => {
        ()
    };

    (@ret $ret:ty) => {
        $ret
    };
}

pub trait FunctionPointer<'a> {
    fn new(ptr: &'a u8) -> Self;
}

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
        $crate::gen_constant_groups!(exp Self => $($name = [$($elem)*];)*);
    };

    (exp $ty:ty, $wrap:ident => $($name:ident = [$($elem:ident)*];)*) => {
        $(
            pub const $name: &'static [$wrap<$ty>] = &[$(Self::$elem),*];
        )*
    };
}

#[macro_export]
macro_rules! gen_v_ref_constants {
    ($($ident:ident)+) => {
        $crate::gen_v_ref_constants!(exp Self => $($ident)*);
    };

    (exp $ty:ty => $($ident:ident)*) => {
        $crate::gen_v_ref_constants!($ty => (0) $($ident)*);
        $crate::gen_constant_groups!(exp $ty, VRef => ALL = [$($ident)*];);
    };

    ($ty:ty => ($prev:expr) $current:ident $($next:ident $($others:ident)*)?) => {
        pub const $current: VRef<$ty> = unsafe { VRef::new($prev) };
        $(
            $crate::gen_v_ref_constants!($ty => (Self::$current.index() + 1) $next $($others)*);
        )?
    };
}

#[macro_export]
macro_rules! gen_frag_ref_constants {
    ($($ident:ident)+) => {
        $crate::gen_frag_ref_constants!(exp Self => $($ident)*);
    };

    (exp $ty:ty => $($ident:ident)*) => {
        $crate::gen_frag_ref_constants!($ty => (0) $($ident)*);
        $crate::gen_constant_groups!(exp $ty, FragRef => ALL = [$($ident)*];);
    };

    ($ty:ty => ($prev:expr) $current:ident $($next:ident $($others:ident)*)?) => {
        pub const $current: FragRef<$ty> = unsafe { FragRef::new(FragAddr::new($prev as u32, 0)) };
        $(
            $crate::gen_frag_ref_constants!($ty => ($prev + 1) $next $($others)*);
        )?
    };
}

#[macro_export]
macro_rules! bitflags {
    (
        $(
            $(#[$($attr:ident),*])?
            $name:ident: $repr:ty {
                $($first:ident $($item:ident)*)?
            }
        )*
    ) => {
        $(
            $crate::bitflags::bitflags! {
                #[derive(Default, Serialize, Deserialize, Archive, $($($attr),*)?)]

                pub struct $name: $repr {

                }
            }

            impl $name {
                $(
                    bitflags!(__fields__ (1, $name), $first $($item)*);
                )?
            }

            impl_flag_and_bool!($name);
        )*
    };

    (__fields__ ($prev:expr, $repr:ident), $current:ident $($next:ident $($other:ident)*)?) => {
        pub const $current: $repr = $repr { bits: $prev };
        $( bitflags!(__fields__ (Self::$current.bits << 1, $repr), $next $($other)*); )?
    }
}

#[macro_export]
macro_rules! compose_error {
    (
        $name:ident {$(
            #[$($fmt:tt)*]
            $variant:ident$(($inner_name:ident: $inner:ty))?,
        )*}
    ) => {
        #[derive(Debug)]
        pub enum $name {$(
            $variant$(($inner))?,
        )*}

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match *self {$(
                    Self::$variant$((ref $inner_name))? => write!(f, $($fmt)*)?,
                )*}
                Ok(())
            }
        }

        impl std::error::Error for $name {}
    };
}

pub extern crate bitflags;

/// Set of virtual pointers. Compared to [`Vec`]<[`bool`]> it uses 8x less memory.
mod bit_set;
/// Trait representing reusable object.
mod clear;
/// Homogenous stack with frame markers.
mod frames;
/// String to uid mapping.
mod interner;
/// HashMap wrapper to work with [`Ident`].
mod map;
/// Map with addressable values. (by [`VPtr`])
mod ordered_map;
/// Similar to ordered map but some indexes may not have mapping.
mod partial_ordered_map;
/// Vector abstraction that allows reusing allocations while preserving other values.
mod pool_map;
mod push_map;
mod rw_swap;
/// Storage that can map additional info for existing map.
mod shadow_map;

pub use {
    bit_set::BitSet,
    bump_alloc::*,
    clear::{map_in_place, Clear},
    frames::Frames,
    interner::{Ident, Interner, InternerArchiver, InternerBase},
    ordered_map::OrderedMap,
    partial_ordered_map::PartialOrderedMap,
    pool_map::PoolMap,
    push_map::PushMap,
    rw_swap::{RWSwapReadAccess, RWSwapReader, RWSwapWriter},
    shadow_map::ShadowMap,
    smallvec::*,
};
