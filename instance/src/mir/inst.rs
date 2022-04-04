use cranelift_codegen::packed_option::PackedOption;

use super::value::Value;

pub struct Ent {
    pub prev: PackedOption<Inst>,
    pub next: PackedOption<Inst>,
    pub kind: Kind,
}

pub enum Kind {
    Return(Value),
}

typec::impl_linked_node!(inout Inst, Ent);
lexer::gen_entity!(Inst);
