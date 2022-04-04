use cranelift_entity::packed_option::PackedOption;
use lexer::prelude::Span;

use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub struct Ent {
    pub prev: PackedOption<Inst>,
    pub next: PackedOption<Inst>,
    pub kind: Kind,
    pub span: Span,
    pub result: PackedOption<Value>,
}

impl Ent {
    pub fn new(kind: Kind, value: Option<Value>, span: Span) -> Self {
        Self {
            prev: None.into(),
            next: None.into(),
            kind,
            span,
            result: value.into(),
        }
    }
}

crate::impl_linked_node!(inout Inst, Ent);

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    IntLit,
    Return,
}

impl Kind {
    pub fn is_terminating(&self) -> bool {
        matches!(self, Kind::Return)
    }
}

lexer::gen_entity!(Inst);
