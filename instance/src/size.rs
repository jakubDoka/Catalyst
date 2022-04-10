#[derive(Debug, Clone, Copy)]
pub struct Size {
    pub arch32: i32,
    pub arch64: i32,
}

impl Size {
    pub const ZERO: Size = Size {
        arch32: 0,
        arch64: 0,
    };
    pub const PTR: Size = Size {
        arch32: 4,
        arch64: 8,
    };

    pub const fn new(arch32: i32, arch64: i32) -> Self {
        Size { arch32, arch64 }
    }

    pub fn arch(&self, arch32: bool) -> i32 {
        if arch32 {
            self.arch32
        } else {
            self.arch64
        }
    }
}

macro_rules! impl_op {
    ($($name:ident-$method:ident($op:tt)),*) => {
        $(
            impl std::ops::$name<Size> for Size {
                type Output = Size;

                fn $method(self, other: Size) -> Size {
                    Size {
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
