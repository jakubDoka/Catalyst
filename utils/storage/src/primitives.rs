use std::{fmt::Debug, marker::PhantomData};

macro_rules! gen_derives {
    ($ident:ident) => {
        impl<T> Clone for $ident<T> {
            fn clone(&self) -> Self {
                Self(self.0, PhantomData)
            }
        }

        impl<T> Copy for $ident<T> {}

        impl<T> Debug for $ident<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}<{}>({})",
                    stringify!($ident),
                    std::any::type_name::<T>(),
                    self.0
                )
            }
        }

        impl<T> PartialEq for $ident<T> {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl<T> Eq for $ident<T> {}

        impl<T> std::hash::Hash for $ident<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }
    };
}

use crate::Invalid;

pub trait VRefDefault: Sized {
    fn default_state() -> VRef<Self>;
}

pub struct VRef<T>(u32, PhantomData<*const T>);

gen_derives!(VRef);

impl<T> VRef<T> {
    #[inline(always)]
    pub const unsafe fn new(id: usize) -> Self {
        Self(id as u32, PhantomData)
    }

    #[inline(always)]
    pub const fn index(self) -> usize {
        self.0 as usize
    }

    pub unsafe fn cast<V>(self) -> VRef<V> {
        std::mem::transmute(self)
    }
}

impl<T> Invalid for VRef<T> {
    #[inline(always)]
    unsafe fn invalid() -> Self {
        Self(u32::MAX, PhantomData)
    }

    #[inline(always)]
    fn is_invalid(&self) -> bool {
        self.0 == u32::MAX
    }
}

impl<T: VRefDefault> Default for VRef<T> {
    fn default() -> Self {
        T::default_state()
    }
}

pub struct VSlice<T>(u32, PhantomData<*const T>);

impl<T> VSlice<T> {
    #[inline(always)]
    pub unsafe fn new(id: usize) -> Self {
        Self(id as u32, PhantomData)
    }

    #[inline(always)]
    pub fn index(self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0 == u32::MAX
    }

    #[inline(always)]
    pub fn empty() -> Self {
        Self(u32::MAX, PhantomData)
    }
}

impl<T> Default for VSlice<T> {
    fn default() -> Self {
        Self::empty()
    }
}

gen_derives!(VSlice);
