use cranelift_codegen::ir::Type;

use crate::size::Size;

#[derive(Debug)]
pub struct Ent {
    pub repr: Type,
    pub offset: Size,
    pub flags: Flags,
}

impl Ent {
    pub fn new(repr: Type, offset: Size, flags: Flags) -> Ent {
        Ent {
            repr,
            offset,
            flags,
        }
    }

    pub fn repr(repr: Type) -> Self {
        Self::new(repr, Size::ZERO, Flags::default())
    }

    pub fn offset(repr: Type, offset: Size) -> Self {
        Self::new(repr, offset, Flags::default())
    }

    pub fn flags(repr: Type, flags: Flags) -> Self {
        Self::new(repr, Size::ZERO, flags)
    }
}

lexer::gen_entity!(Value);

#[derive(Debug, Default)]
pub struct Flags(u8);

impl std::ops::BitOr for Flags {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

macro_rules! gen_flag_getter {
    ($($name:ident $const_name:ident $index:literal,)*) => {
        impl Flags {
            $(
                pub const $const_name: Self = Self(1 << $index);
            )*
            
            $(
                pub fn $name(&self) -> bool {
                    self.0 & (1 << $index) != 0
                }
            )*
        }
    };
}

gen_flag_getter!(
    is_signed SIGNED 0,
    is_pointer POINTER 1,
);