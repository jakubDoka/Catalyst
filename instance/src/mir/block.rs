use cranelift_codegen::{packed_option::PackedOption};
use cranelift_entity::EntityList;

use super::{Value, Inst};

#[derive(Default, Clone, Copy, Debug)]
pub struct Ent {
    pub prev: PackedOption<Block>,
    pub next: PackedOption<Block>,
    pub first: PackedOption<Inst>,
    pub last: PackedOption<Inst>,
    pub params: EntityList<Value>,
}

typec::impl_linked_node!(inout Block, Ent);
lexer::gen_entity!(Block);
