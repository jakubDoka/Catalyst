
use storage::*;
use cranelift_codegen::ir::Type;
use typec_types::*;
use crate::*;

pub struct Reprs {
    pub fields: Map<ReprField>,
    pub ents: SecondaryMap<Ty, ReprEnt>,
}

impl Reprs {
    pub fn new() -> Self {
        Reprs {
            fields: Map::new(),
            ents: SecondaryMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ReprEnt {
    pub repr: Type,
    pub size: Size,
    pub align: Size,
    pub flags: ReprFlags,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ReprField {
    pub offset: Size,
}

impl ReservedValue for ReprField {
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
    pub struct ReprFlags: u32 {
        /// This type cannot fit into register.
        const ON_STACK = 1 << 0;
        /// This type can be safely copied.
        const COPYABLE = 1 << 1;
    }
}

impl_bool_bit_and!(ReprFlags);