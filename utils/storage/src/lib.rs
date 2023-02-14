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
macro_rules! wrapper_enum {
    (
        $(#[$meta:meta])*
        enum $name:ident: $($relocated:ident)? {
            $(
                $variant:ident: $ty:ty $(=> $readable_name:literal)?,
            )*
        }
    ) => {
        $(#[$meta])*
        pub enum $name {
            $(
                $variant($ty),
            )*
        }

        $(
            impl From<$ty> for $name {
                fn from(value: $ty) -> Self {
                    Self::$variant(value)
                }
            }
        )*

        impl $name {
            pub const fn name(&self) -> &'static str {
                match *self {
                    $(
                        Self::$variant(..) => wrapper_enum!($variant $($readable_name)?),
                    )*
                }
            }
        }

        wrapper_enum!(@relocated $($relocated)? enum $name { $($variant(a) => a,)* });
    };

    ($variant:ident) => {
        stringify!($variant)
    };

    ($variant:ident $readable_name:literal) => {
        $readable_name
    };

    (@relocated relocated enum $($tt:tt)*) => {
        derive_relocated!(enum $($tt)*);
    };

    (@relocated $($tt:tt)*) => {};
}

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

#[macro_export]
macro_rules! transmute_arkive {
    ($($name:ident($ty:ty => $repr:ty))*) => {
        $(
            transmute_arkive!($name, $ty, $repr);
        )*
    };

    ($name:ident, $ty:ty, $repr:ty) => {
        pub struct $name;

        impl ArchiveWith<$ty> for $name {
            type Archived = Archived<$repr>;

            type Resolver = Resolver<$repr>;

            unsafe fn resolve_with(
                field: &$ty,
                pos: usize,
                resolver: Self::Resolver,
                out: *mut Self::Archived,
            ) {
                mem::transmute_copy::<_, $repr>(field).resolve(pos, resolver, out)
            }
        }

        impl<S: Serializer + ?Sized> SerializeWith<$ty, S> for $name {
            fn serialize_with(
                field: &$ty,
                serializer: &mut S,
            ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
                // SAFETY: ther is none
                unsafe { mem::transmute_copy::<_, $repr>(field).serialize(serializer) }
            }
        }

        impl<D: Fallible + ?Sized> DeserializeWith<Archived<$repr>, $ty, D> for $name {
            fn deserialize_with(
                field: &Archived<$repr>,
                _deserializer: &mut D,
            ) -> Result<$ty, <D as rkyv::Fallible>::Error> {
                Ok(unsafe { mem::transmute_copy::<_, $ty>(field) })
            }
        }
    };
}

pub extern crate bitflags;

mod bit_set;
mod clear;
mod frames;
mod map;
mod ordered_map;
mod partial_ordered_map;
mod pool_map;
mod push_map;
mod rw_swap;
mod shadow_map;

pub use {
    bit_set::BitSet,
    bump_alloc::*,
    clear::{map_in_place, Clear},
    frames::Frames,
    ordered_map::OrderedMap,
    partial_ordered_map::PartialOrderedMap,
    pool_map::PoolMap,
    push_map::{PushMap, PushMapCheck, PushMapView},
    rw_swap::{RWSwapReadAccess, RWSwapReader, RWSwapWriter},
    shadow_map::ShadowMap,
    smallvec::*,
};
