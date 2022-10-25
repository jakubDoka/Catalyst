use std::{
    fmt::Debug,
    marker::PhantomData,
    mem::{discriminant, transmute},
};

#[repr(C, u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CtlOption<T> {
    None,
    Some(T),
}

impl<T> From<Option<T>> for CtlOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            None => CtlOption::None,
            Some(value) => CtlOption::Some(value),
        }
    }
}

const _: () = {
    assert!(unsafe { transmute::<_, u8>(discriminant(&CtlOption::<()>::None)) } == 0);
    assert!(unsafe { transmute::<_, u8>(discriminant(&CtlOption::<()>::Some(()))) } == 1);
};

impl<T> From<CtlOption<T>> for Option<T> {
    fn from(value: CtlOption<T>) -> Self {
        match value {
            CtlOption::None => None,
            CtlOption::Some(value) => Some(value),
        }
    }
}

pub trait TransposeOption: Sized {
    fn transpose(self) -> Self;
}

impl<T> TransposeOption for Option<Option<T>> {
    fn transpose(self) -> Self {
        match self {
            Some(Some(x)) => Some(Some(x)),
            Some(None) => None,
            None => Some(None),
        }
    }
}

pub trait NoShortCircuitCollect {
    type Item;
    fn nsc_collect<T: FromIterator<Self::Item>>(self) -> T;
}

impl<E, I> NoShortCircuitCollect for I
where
    I: Iterator<Item = E>,
{
    type Item = E;

    #[inline]
    fn nsc_collect<T: FromIterator<Self::Item>>(mut self) -> T {
        let res = self.by_ref().collect();
        self.for_each(drop);
        res
    }
}

macro_rules! gen_derives {
    ($ident:ident) => {
        impl<T: ?Sized> Clone for $ident<T> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<T: ?Sized> Copy for $ident<T> {}

        impl<T: ?Sized> Debug for $ident<T> {
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

        impl<T: ?Sized> const PartialEq for $ident<T> {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl<T: ?Sized> const Eq for $ident<T> {}

        impl<T: ?Sized> std::hash::Hash for $ident<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }
    };
}

use serde::{Deserialize, Serialize};

pub type VRefSlice<T> = VSlice<VRef<T>>;

pub trait VRefDefault {
    fn default_state() -> VRef<Self>;
}

#[rustc_layout_scalar_valid_range_end(4294967294)]
#[repr(transparent)]
pub struct VRef<T: ?Sized>(u32, PhantomData<*const T>);

unsafe impl<T: ?Sized> Send for VRef<T> {}
unsafe impl<T: ?Sized> Sync for VRef<T> {}

pub type OptVRef<T> = Option<VRef<T>>;

gen_derives!(VRef);

impl<T: ?Sized> VRef<T> {
    /// Creates new VRef from index.
    /// # Safety
    /// The index must be valid for using collection.
    #[inline(always)]
    pub const unsafe fn new(id: usize) -> Self {
        Self(id as u32, PhantomData)
    }

    #[inline(always)]
    pub const fn index(self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    /// Casts VRef to VRef of another type.
    /// # Safety
    /// The index must be valid for using collection or cannot
    /// be used in any collection.
    #[inline(always)]
    pub unsafe fn cast<V: ?Sized>(self) -> VRef<V> {
        std::mem::transmute(self)
    }
}

impl<T: ?Sized> Ord for VRef<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T: ?Sized> PartialOrd for VRef<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: VRefDefault + ?Sized> Default for VRef<T> {
    #[inline(always)]
    fn default() -> Self {
        T::default_state()
    }
}

impl<T: ?Sized> Serialize for VRef<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

impl<'de, T: ?Sized> Deserialize<'de> for VRef<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        u32::deserialize(deserializer).map(|x| unsafe { Self::new(x as usize) })
    }
}

pub struct VSlice<T: ?Sized>(u32, PhantomData<*const T>);

unsafe impl<T: ?Sized> Send for VSlice<T> {}
unsafe impl<T: ?Sized> Sync for VSlice<T> {}

impl<T> VSlice<T> {
    /// Creates new VSlice from index.
    /// # Safety
    /// The index must be valid for using collection.
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
    pub const fn empty() -> Self {
        Self(u32::MAX, PhantomData)
    }
}

impl<T> Default for VSlice<T> {
    fn default() -> Self {
        Self::empty()
    }
}

gen_derives!(VSlice);
