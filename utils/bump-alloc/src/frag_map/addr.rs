use std::mem;

macro_rules! gen_non_max {
    ($($name:ident($ty:ty, $max:literal);)*) => {
        $(
            #[repr(transparent)]
            #[rustc_layout_scalar_valid_range_end($max)]
            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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

                pub const fn repr(self) -> $ty {
                    self.0
                }

                pub const fn new(val: $ty) -> Option<Self> {
                    unsafe { mem::transmute(val) }
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
        )*
    };
}

gen_non_max!(
    NonMaxU64(u64, /* 1 << 64 - 2 */ 18_446_744_073_709_551_614);
    NonMaxU32(u32, /* 1 << 32 - 2 */ 4_294_967_294);
);

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct FragAddr(NonMaxU64);

impl FragAddr {
    pub const fn new(global: u64, local: u8) -> Self {
        let encoded = Self::encode(global, local);
        // Safety: this will never happen as encode never returns `MAX`
        unsafe { Self(NonMaxU64::new_unchecked(encoded)) }
    }

    pub const fn encode(global: u64, local: u8) -> u64 {
        FragSliceAddr::encode(global, local, 1)
    }

    pub const fn decode(repr: u64) -> (u64, u8) {
        let (index, thread, ..) = FragSliceAddr::decode(repr);
        (index, thread)
    }

    pub const fn repr(self) -> u64 {
        self.0.get()
    }

    pub const fn parts(self) -> (u64, u8) {
        Self::decode(self.repr())
    }

    pub const fn as_slice(self) -> FragSliceAddr {
        FragSliceAddr(self.0.get())
    }

    pub const fn right_after(self, other: FragAddr) -> bool {
        let (index, thread) = self.parts();
        let (other_index, other_thread) = other.parts();
        thread == other_thread && index == other_index + 1
    }

    pub const fn thread(&self) -> u8 {
        self.parts().1
    }
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug, Default)]
pub struct FragSliceAddr(u64);

impl FragSliceAddr {
    const INDEX_OFFSET: u64 = 24;
    const LEN_OFFSET: u64 = 8;
    const THREAD_MASK: u64 = (1 << Self::LEN_OFFSET) - 1;
    const LEN_MASK: u64 = (1 << (Self::INDEX_OFFSET - Self::LEN_OFFSET)) - 1;

    pub const fn new(index: u64, thread: u8, len: u16) -> Self {
        let encoded = Self::encode(index, thread, len);
        // Safety: we assume so big allocation is impossible
        Self(encoded)
    }

    pub const fn encode(index: u64, thread: u8, len: u16) -> u64 {
        (index << Self::INDEX_OFFSET)
            | (len as u64 & Self::LEN_MASK) << Self::LEN_OFFSET
            | (thread as u64 & Self::THREAD_MASK)
    }

    pub const fn decode(repr: u64) -> (u64, u8, u16) {
        let local = (repr & Self::THREAD_MASK) as u8;
        let len = ((repr >> Self::LEN_OFFSET) & Self::LEN_MASK) as u16;
        let global = repr >> Self::INDEX_OFFSET;
        (global, local, len)
    }

    pub const fn parts(self) -> (u64, u8, u16) {
        Self::decode(self.0)
    }

    pub const fn repr(self) -> u64 {
        self.0
    }

    pub(crate) fn keys(
        &self,
    ) -> impl Iterator<Item = FragAddr> + ExactSizeIterator + DoubleEndedIterator {
        let (index, thread, len) = self.parts();
        (index as usize..index as usize + len as usize)
            .map(move |index| FragAddr::new(index as u64, thread))
    }
}
