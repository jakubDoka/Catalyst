use std::path::{Path, PathBuf};

use crate::scope;
use lexer::*;
use storage::*;

pub type ModuleMap = Map<Source>;
pub type Modules = SecondaryMap<Source, ModuleEnt>;

#[derive(Debug, Clone, Default)]
pub struct ModuleEnt {
    pub id: ID,
    pub ordering: usize,
    pub path: PathBuf,
    pub dependency: Vec<Source>,
    pub items: Vec<ModuleItem>,
}

impl ModuleEnt {
    pub fn new(id: ID, path: &Path) -> Self {
        Self {
            id,
            ordering: usize::MAX,
            path: path.to_path_buf(),
            dependency: Vec::new(),
            items: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
