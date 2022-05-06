
pub(self) use storage::*;
use cranelift_codegen::ir::Type;
pub(self) use typec_types::*;
use crate::*;

pub struct Types {
    pub fields: Map<Field>,
    pub ents: SecondaryMap<Ty, Ent>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            fields: Map::new(),
            ents: SecondaryMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Ent {
    pub repr: Type,
    pub size: Size,
    pub align: Size,
    pub flags: Flags,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Field {
    pub offset: Size,
}

impl ReservedValue for Field {
    fn reserved_value() -> Self {
        Self {
            offset: Size::new(i32::MAX, i32::MAX),
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.offset == Size::new(i32::MAX, i32::MAX)
    }
}

bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        /// This type cannot fit into register.
        const ON_STACK = 1 << 0;
        /// This type can be safely copied.
        const COPYABLE = 1 << 1;
    }
}

impl_bool_bit_and!(Flags);