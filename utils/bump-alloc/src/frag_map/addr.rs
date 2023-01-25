use std::mem;

use rkyv::{Archive, Archived, Deserialize, Fallible, Serialize};

macro_rules! gen_non_max {
    ($($name:ident($ty:ty, $max:literal);)*) => {
        $(
            #[repr(transparent)]
            #[rustc_layout_scalar_valid_range_end($max)]
            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Archive, Serialize)]
            #[archive_attr(derive(PartialEq, Eq, Hash))]
            pub struct $name($ty);

            impl Default for $name {
                fn default() -> Self {
                    Self::MIN
                }
            }

            impl $name {
                pub const MAX: Self = unsafe { Self($max) };
                pub const MIN: Self = unsafe { Self(0) };

                pub const fn get(self) -> $ty {
                    self.0
                }

                pub const fn addr(self) -> $ty {
                    self.0
                }

                pub const fn new(val: $ty) -> Option<Self> {
                    if val < $max {
                        Some(unsafe { Self(val) })
                    } else {
                        None
                    }
                }

                /// # Safety
                /// The inputted type must not be equal to `type::MAX`
                pub const unsafe fn new_unchecked(val: $ty) -> Self {
                    debug_assert!(val != <$ty>::MAX);
                    unsafe { Self(val) }
                }
            }

            const _: () = assert!($name::new(<$ty>::MAX).is_none());
            const _: () = assert!(mem::size_of::<Option<$name>>() == mem::size_of::<$name>());


            impl<D: Fallible> Deserialize<$name, D> for Archived<$name>
            where
                D::Error: From<NonMaxError>,
            {
                fn deserialize(&self, _deserializer: &mut D) -> Result<$name, <D as Fallible>::Error> {
                    $name::new(self.0)
                        .ok_or(NonMaxError)
                        .map_err(Into::into)
                }
            }
        )*


    };
}

gen_non_max!(
    NonMaxU64(u64, /* 1 << 64 - 2 */ 18_446_744_073_709_551_614);
    NonMaxU32(u32, /* 1 << 32 - 2 */ 4_294_967_294);
    NonMaxU16(u16, /* 1 << 16 - 2 */ 65_534);
);

#[derive(Debug)]
pub struct NonMaxError;

#[derive(
    Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug, Archive, Serialize, Deserialize,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct FragAddr {
    pub index: u32,
    pub thread: u8,
    invalid: bool,
}

impl FragAddr {
    pub const fn new(index: u32, thread: u8) -> Self {
        Self {
            index,
            thread,
            invalid: false,
        }
    }

    pub const fn right_after(self, other: FragAddr) -> bool {
        self.thread == other.thread && self.index == other.index.wrapping_add(1)
    }

    pub const fn as_slice(&self) -> FragSliceAddr {
        FragSliceAddr {
            index: self.index,
            thread: self.thread,
            len: 1,
        }
    }

    pub const fn addr(self) -> Self {
        self
    }
}

#[derive(
    Copy,
    Clone,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
    Hash,
    Debug,
    Default,
    Archive,
    Serialize,
    Deserialize,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct FragSliceAddr {
    pub index: u32,
    pub thread: u8,
    pub len: u16,
}

impl FragSliceAddr {
    pub const fn new(index: u32, thread: u8, len: u16) -> Self {
        Self { index, thread, len }
    }

    pub(crate) fn keys(
        self,
    ) -> impl Iterator<Item = FragAddr> + ExactSizeIterator + DoubleEndedIterator {
        (self.index as usize..self.index as usize + self.len as usize)
            .map(move |index| FragAddr::new(index as u32, self.thread))
    }

    pub const fn addr(self) -> Self {
        self
    }
}
