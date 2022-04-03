use cranelift_entity::packed_option::PackedOption;
use parser::ast::Ast;

use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub struct Ent {
    pub prev: PackedOption<Inst>,
    pub next: PackedOption<Inst>,
    pub kind: Kind,
    pub ast: Ast,
    pub result: PackedOption<Value>,
}

impl Ent {
    pub fn new(kind: Kind, value: Option<Value>, ast: Ast) -> Self {
        Self {
            prev: None.into(),
            next: None.into(),
            kind,
            ast,
            result: value.into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    Return,
}

impl Kind {
    pub fn is_terminating(&self) -> bool {
        matches!(self, Kind::Return)
    }
}

lexer::gen_entity!(Inst);