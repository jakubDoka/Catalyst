use cranelift_entity::{packed_option::PackedOption, EntityList};

use super::{value::Value, inst::Inst};


#[derive(Default)]
pub struct Ent {
    pub args: EntityList<Value>,
    pub prev: PackedOption<Block>,
    pub next: PackedOption<Block>,
    pub first: PackedOption<Inst>,
    pub last: PackedOption<Inst>,
}

lexer::gen_entity!(Block);