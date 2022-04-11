use cranelift_codegen::packed_option::PackedOption;
use cranelift_entity::EntityList;
use typec::Func;

use super::{value::Value, Block};

#[derive(Debug)]
pub struct Ent {
    pub prev: PackedOption<Inst>,
    pub next: PackedOption<Inst>,
    pub kind: Kind,
    pub value: PackedOption<Value>,
}

impl Ent {
    pub fn valueless(kind: Kind) -> Self {
        Self::new(kind, None)
    }

    pub fn with_value(kind: Kind, value: Value) -> Self {
        Self::new(kind, value.into())
    }

    pub fn new(kind: Kind, value: Option<Value>) -> Self {
        Self {
            prev: None.into(),
            next: None.into(),
            kind,
            value: value.into(),
        }
    }
}

#[derive(Debug)]
pub enum Kind {
    Copy(Value),
    Variable,
    Assign(Value),
    JumpIfFalse(Block),
    Jump(Block),
    Call(Func, EntityList<Value>),
    IntLit(u64),
    BoolLit(bool),
    Return,
}

typec::impl_linked_node!(inout Inst, Ent);
lexer::gen_entity!(Inst);
