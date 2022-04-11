use cranelift_codegen::{
    ir::{types, Type},
    packed_option::{PackedOption, ReservedValue},
};
use cranelift_entity::SecondaryMap;
use lexer::Map;
use typec::Ty;

use crate::size::Size;

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
    pub small: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Field {
    pub repr: Type,
    pub offset: Size,
}

impl ReservedValue for Field {
    fn reserved_value() -> Self {
        Self {
            repr: types::INVALID,
            offset: Size::ZERO,
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.repr == types::INVALID && self.offset == Size::ZERO
    }
}
