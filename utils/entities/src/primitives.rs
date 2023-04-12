use std::{
    fmt::Debug,
    marker::PhantomData,
    mem::{discriminant, transmute},
    ops::{Index, Range},
};

use crate::{frag_map::addr::NonMaxU32, BumpVec, FragAddr, FragSliceAddr, ToBumpVec};

macro_rules! gen_derives {
    ($name:ident, $repr_getter:path) => {
        impl<T> Clone for $name<T> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<T> Copy for $name<T> {}

        impl<T> Debug for $name<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}<{}>({:?})",
                    stringify!($name),
                    std::any::type_name::<T>(),
                    $repr_getter(*self)
                )
            }
        }

        impl<T> PartialEq for $name<T> {
            fn eq(&self, other: &Self) -> bool {
                $repr_getter(*self) == $repr_getter(*other)
            }
        }

        impl<T> Eq for $name<T> {}

        impl<T> std::hash::Hash for $name<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                $repr_getter(*self).hash(state);
            }
        }

        impl<T> PartialOrd for $name<T> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                $repr_getter(*self).partial_cmp(&$repr_getter(*other))
            }
        }

        impl<T> Ord for $name<T> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                $repr_getter(*self).cmp(&$repr_getter(*other))
            }
        }

        unsafe impl<T> Send for $name<T> {}
        unsafe impl<T> Sync for $name<T> {}
    };
}

pub type OptFragRef<T> = Option<FragRef<T>>;
pub type FragRefSlice<T> = FragSlice<FragRef<T>>;
pub type OptVRef<T> = Option<VRef<T>>;
pub type VRefSlice<T> = VSlice<VRef<T>>;

/// An id pointing to some item in frag storage family. Wrapper aroung untyped `FragAddr`.
#[repr(transparent)]

pub struct FragRef<T>(pub(crate) FragAddr, pub(crate) PhantomData<*const T>);
gen_derives!(FragRef, Self::addr);

impl<T> FragRef<T> {
    pub const fn new(addr: FragAddr) -> Self {
        Self(addr, PhantomData)
    }

    /// Converts ref to slice with length 1.
    pub const fn as_slice(self) -> FragSlice<T> {
        FragSlice::new(self.0.as_slice())
    }

    /// Returns inner adress.
    pub const fn addr(self) -> FragAddr {
        self.0
    }

    /// returns self encoded into u64. Encoding tries to make resulting value as small as possible.
    pub const fn bits(self) -> u64 {
        (self.0.index as u64) << 8 | self.0.thread as u64
    }
}

/// An id pointing to 0..n items in frag storage family. Wrapper aroung untyped `FragSliceAddr`.

pub struct FragSlice<T>(pub(crate) FragSliceAddr, pub(crate) PhantomData<*const T>);
gen_derives!(FragSlice, Self::addr);

impl<T> FragSlice<T> {
    pub const fn new(addr: FragSliceAddr) -> Self {
        Self(addr, PhantomData)
    }

    pub const fn len(&self) -> usize {
        self.0.len as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns iterator over adresses inside the slice.
    pub fn keys(
        &self,
    ) -> impl Iterator<Item = FragRef<T>> + DoubleEndedIterator + ExactSizeIterator {
        self.0.keys().map(|addr| FragRef(addr, PhantomData))
    }

    /// Projects slice local index to collection global index.
    pub fn index(self, index: usize) -> FragRef<T> {
        self.keys().nth(index).expect("index out of bounds")
    }

    pub fn empty() -> Self {
        Self(FragSliceAddr::default(), PhantomData)
    }

    pub fn to_bumpvec(self, ctx: &impl Index<Self, Output = [T]>) -> BumpVec<T>
    where
        T: Clone,
    {
        ctx[self].to_bumpvec()
    }

    /// Returns inner adress.
    pub const fn addr(self) -> FragSliceAddr {
        self.0
    }
}

impl<T> Default for FragSlice<T> {
    fn default() -> Self {
        Self(FragSliceAddr::default(), PhantomData)
    }
}

/// Virtual pointer of just an index to some collection.
#[repr(transparent)]

pub struct VRef<T>(NonMaxU32, PhantomData<*const T>);
gen_derives!(VRef, Self::addr);

const _: () = {
    assert!(std::mem::size_of::<Option<VRef<()>>>() == std::mem::size_of::<u32>());
};

impl<T> VRef<T> {
    /// Creates new VRef from index.
    #[inline(always)]
    pub const fn new(id: usize) -> Self {
        // SAFETY: considering how vrefs are used, this is safe in the same way UUIDs are safe.
        Self(unsafe { NonMaxU32::new_unchecked(id as u32) }, PhantomData)
    }

    #[inline(always)]
    pub const fn index(self) -> usize {
        self.0.get() as usize
    }

    #[inline(always)]
    pub const fn addr(self) -> u32 {
        self.0.get()
    }

    /// Casts VRef to VRef of another type.
    #[inline(always)]
    pub fn cast<V>(self) -> VRef<V> {
        unsafe { std::mem::transmute(self) }
    }
}

/// Virtual slice, usually attached to some collection. Uses indexing instead of pointers.

pub struct VSlice<T>(u32, u32, PhantomData<*const T>);
gen_derives!(VSlice, Self::addr);

impl<T> VSlice<T> {
    /// Creates new VSlice from index.
    /// # Safety
    /// The index must be valid for using collection.
    pub fn new(range: Range<usize>) -> Self {
        Self(range.start as u32, range.end as u32, PhantomData)
    }

    pub fn range(self) -> Range<usize> {
        self.0 as usize..self.1 as usize
    }

    pub fn addr(self) -> (u32, u32) {
        (self.0, self.1)
    }

    pub fn len(self) -> usize {
        (self.1 - self.0) as usize
    }

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    pub const fn empty() -> Self {
        Self(0, 0, PhantomData)
    }

    pub fn keys(self) -> impl Iterator<Item = VRef<T>> {
        self.range().map(|i| VRef::new(i))
    }

    pub fn index(self, index: usize) -> VRef<T> {
        self.keys().nth(index).expect("Index out of bounds.")
    }

    pub fn start(self) -> u32 {
        self.0
    }

    pub fn slice(self, range: Range<usize>) -> Self {
        assert!(range.start <= range.end);
        assert!(range.end <= self.len());
        let start = self.0 + range.start as u32;
        let end = self.0 + range.end as u32;
        Self(start, end, PhantomData)
    }
}

impl<T> Default for VSlice<T> {
    fn default() -> Self {
        Self::empty()
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

/// Option type that has same layout as Catalyst Option type.
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
