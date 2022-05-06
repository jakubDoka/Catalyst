use storage::*;
use lexer_types::*;
use crate::scope;

pub type Modules = SecondaryMap<Source, ModuleEnt>;

#[derive(Debug, Clone, Default)]
pub struct ModuleEnt {
    pub id: ID,
    pub items: Vec<ModuleItem>,
}

impl ModuleEnt {
    pub fn new(id: ID) -> Self {
        Self {
            id,
            items: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleItem {
    pub id: ID,
    pub kind: scope::ScopePointer,
    pub span: Span,
}

impl ModuleItem {
    pub fn new(id: ID, kind: impl EntityRef + 'static, span: Span) -> Self {
        Self {
            id,
            kind: scope::ScopePointer::write(kind),
            span,
        }
    }

    pub fn to_scope_item(&self) -> scope::ScopeItem {
        scope::ScopeItem {
            span: self.span,
            pointer: self.kind,
        }
    }
}