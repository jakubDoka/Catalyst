use std::{
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    mem::{discriminant, transmute},
    ops::Range,
};

use bytecheck::CheckBytes;
use rkyv::with::Skip;

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
                    "{}<{}>({:?})",
                    stringify!($ident),
                    std::any::type_name::<T>(),
                    self.0
                )
            }
        }

        impl<T: ?Sized> PartialEq for $ident<T> {
            fn eq(&self, other: &Self) -> bool {
                self.0.addr() == other.0.addr()
            }
        }

        impl<T: ?Sized> Eq for $ident<T> {}

        impl<T: ?Sized> std::hash::Hash for $ident<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        impl<T: ?Sized> PartialOrd for $ident<T> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                self.0.partial_cmp(&other.0)
            }
        }

        impl<T: ?Sized> Ord for $ident<T> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.cmp(&other.0)
            }
        }

        unsafe impl<T: ?Sized> Send for $ident<T> {}
        unsafe impl<T: ?Sized> Sync for $ident<T> {}
    };
}

#[repr(transparent)]
#[derive(Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub struct FragRef<T: ?Sized>(pub(crate) FragAddr, pub(crate) PhantomData<*const T>);
gen_derives!(FragRef);

impl<T: ?Sized> Hash for ArchivedFragRef<T> {
    fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: ?Sized> PartialEq for ArchivedFragRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for ArchivedFragRef<T> {}

impl<T: ?Sized> FragRef<T> {
    pub const fn new(addr: FragAddr) -> Self {
        Self(addr, PhantomData)
    }

    pub const fn as_slice(self) -> FragSlice<T> {
        FragSlice::new(self.0.as_slice())
    }

    pub const fn right_after(self, key: FragRef<T>) -> bool {
        self.0.right_after(key.0)
    }

    pub const fn addr(self) -> FragAddr {
        self.0
    }

    pub const fn bits(self) -> u64 {
        (self.0.index as u64) << 8 | self.0.thread as u64
    }
}

pub type OptFragRef<T> = Option<FragRef<T>>;

#[derive(Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub struct FragSlice<T: ?Sized>(pub(crate) FragSliceAddr, pub(crate) PhantomData<*const T>);
gen_derives!(FragSlice);

impl<T: ?Sized> Hash for ArchivedFragSlice<T> {
    fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: ?Sized> PartialEq for ArchivedFragSlice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for ArchivedFragSlice<T> {}

pub type FragRefSlice<T> = FragSlice<FragRef<T>>;

impl<T: ?Sized> FragSlice<T> {
    /// # Safety
    /// See [`FragSliceAddr::new`].
    pub const fn new(addr: FragSliceAddr) -> Self {
        Self(addr, PhantomData)
    }

    pub const fn len(&self) -> usize {
        self.0.len as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn keys(
        &self,
    ) -> impl Iterator<Item = FragRef<T>> + DoubleEndedIterator + ExactSizeIterator {
        self.0.keys().map(|addr| FragRef(addr, PhantomData))
    }

    pub fn index(self, index: usize) -> FragRef<T> {
        self.keys().nth(index).expect("index out of bounds")
    }

    pub fn empty() -> Self {
        Self(FragSliceAddr::default(), PhantomData)
    }

    pub const fn addr(self) -> FragSliceAddr {
        self.0
    }
}

impl<T: ?Sized> Default for FragSlice<T> {
    fn default() -> Self {
        Self(FragSliceAddr::default(), PhantomData)
    }
}

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

use rkyv::{Archive, Deserialize, Serialize};

use crate::{frag_map::addr::NonMaxU32, FragAddr, FragSliceAddr};

pub type VRefSlice<T> = VSlice<VRef<T>>;

pub trait VRefDefault {
    fn default_state() -> VRef<Self>;
}

#[repr(transparent)]
#[derive(Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub struct VRef<T: ?Sized>(NonMaxU32, PhantomData<*const T>);

impl<T: ?Sized> Hash for ArchivedVRef<T> {
    fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: ?Sized> PartialEq for ArchivedVRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for ArchivedVRef<T> {}

pub type OptVRef<T> = Option<VRef<T>>;

gen_derives!(VRef);

impl<T: ?Sized> VRef<T> {
    /// Creates new VRef from index.
    /// # Safety
    /// The index must be valid for using collection.
    #[inline(always)]
    pub const fn new(id: usize) -> Self {
        Self(unsafe { NonMaxU32::new_unchecked(id as u32) }, PhantomData)
    }

    #[inline(always)]
    pub const fn index(self) -> usize {
        self.0.get() as usize
    }

    #[inline(always)]
    pub const fn as_u32(self) -> u32 {
        self.0.get()
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

impl<T: VRefDefault + ?Sized> Default for VRef<T> {
    #[inline(always)]
    fn default() -> Self {
        T::default_state()
    }
}

#[const_trait]
trait ReprComply {
    fn addr(self) -> u32;
}

impl const ReprComply for u32 {
    #[inline(always)]
    fn addr(self) -> u32 {
        self
    }
}

#[derive(Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub struct VSlice<T: ?Sized>(u32, u32, #[with(Skip)] PhantomData<*const T>);

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

gen_derives!(VSlice);
