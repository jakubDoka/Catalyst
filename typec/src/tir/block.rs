use cranelift_entity::{packed_option::PackedOption, EntityList};

use super::{inst::Inst, value::Value};

#[derive(Default, Clone, Copy)]
pub struct Ent {
    pub args: EntityList<Value>,
    pub prev: PackedOption<Block>,
    pub next: PackedOption<Block>,
    pub first: PackedOption<Inst>,
    pub last: PackedOption<Inst>,
}

crate::impl_linked_node!(inout Block, Ent);
lexer::gen_entity!(Block);
