use storage::*;
use lexer_types::*;
use crate::scope;

pub type Modules = SecondaryMap<Source, Ent>;

#[derive(Debug, Clone, Default)]
pub struct Ent {
    pub id: ID,
    pub items: Vec<Item>,
}

impl Ent {
    pub fn new(id: ID) -> Self {
        Self {
            id,
            items: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    pub id: ID,
    pub kind: scope::Pointer,
    pub span: Span,
}

impl Item {
    pub fn new(id: ID, kind: impl EntityRef + 'static, span: Span) -> Self {
        Self {
            id,
            kind: scope::Pointer::write(kind),
            span,
        }
    }

    pub fn to_scope_item(&self) -> scope::Item {
        scope::Item {
            span: self.span,
            pointer: self.kind,
        }
    }
}