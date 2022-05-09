
#[derive(Clone, Copy, Default, Debug)]
pub struct Layout {
    pub arch32: u32,
    pub arch64: u32,
}

impl Layout {
    const WIDTH: u32 = std::mem::size_of::<u32>() as u32 * 8;
    const ALIGN_WIDTH: u32 = 3;
    const SIZE_WIDTH: u32 = Self::WIDTH - Self::ALIGN_WIDTH;
    const SIZE_MASK: u32 = (1 << Self::SIZE_WIDTH) - 1;

    pub const ZERO: Self = Self {
        arch32: 0,
        arch64: 0,
    };
    pub const PTR: Self = Self::new(Offset::PTR, Offset::PTR);

    pub const fn new(size: Offset, align: Offset) -> Self {
        Self {
            arch32: (size.arch32 as u32 & Self::SIZE_MASK) 
                | ((align.arch32 as u32) << Self::SIZE_WIDTH),
            arch64: (size.arch64 as u32 & Self::SIZE_MASK) 
                | ((align.arch64 as u32) << Self::SIZE_WIDTH),
        }
    }

    pub fn size(&self) -> Offset {
        Offset::new(
            (self.arch32 & Self::SIZE_MASK) as i32, 
            (self.arch64 & Self::SIZE_MASK) as i32,
        )
    }

    pub fn align(&self) -> Offset {
        Offset::new(
            (self.arch32 >> Self::SIZE_WIDTH) as i32, 
            (self.arch64 >> Self::SIZE_WIDTH) as i32,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Offset {
    pub arch32: i32,
    pub arch64: i32,
}

impl Offset {
    pub const ZERO: Offset = Offset {
        arch32: 0,
        arch64: 0,
    };
    pub const PTR: Offset = Offset {
        arch32: 4,
        arch64: 8,
    };

    pub const fn new(arch32: i32, arch64: i32) -> Self {
        Offset { arch32, arch64 }
    }

    pub fn arch(&self, arch32: bool) -> i32 {
        if arch32 {
            self.arch32
        } else {
            self.arch64
        }
    }

    pub fn max(self, other: Self) -> Self {
        Offset {
            arch32: self.arch32.max(other.arch32),
            arch64: self.arch64.max(other.arch64),
        }
    }

    pub fn min(self, other: Self) -> Self {
        Offset {
            arch32: self.arch32.min(other.arch32),
            arch64: self.arch64.min(other.arch64),
        }
    }
}

macro_rules! impl_op {
    ($($name:ident-$method:ident($op:tt)),*) => {
        $(
            impl std::ops::$name<Offset> for Offset {
                type Output = Offset;

                fn $method(self, other: Offset) -> Offset {
                    Offset {
                        arch32: self.arch32 $op other.arch32,
                        arch64: self.arch64 $op other.arch64,
                    }
                }
            }
        )*
    };
}

impl_op!(
    Add-add(+),
    Sub-sub(-),
    Mul-mul(*),
    Div-div(/),
    Rem-rem(%)
);

impl std::ops::Mul<i32> for Offset {
    type Output = Offset;

    fn mul(self, other: i32) -> Offset {
        Offset {
            arch32: self.arch32 * other,
            arch64: self.arch64 * other,
        }
    }
}
