use std::{any::TypeId, path::PathBuf};

use lexer::{map::ID, Span};
use parser::{AnyError};

use crate::{module::Module, unit::Unit, logic::{Modules, Units}, scope::ScopeItemLexicon};

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    ModuleNotFound(PathBuf),
    ModuleLoadFailed(PathBuf, std::io::Error),
    ModuleCycle(Vec<Module>),
    UnitNotFound(PathBuf),
    UnitLoadFailed(PathBuf, std::io::Error),
    UnitCycle(Vec<Unit>),
    SyntaxError(parser::error::Kind),
    GitClone(std::io::Error),
    MkDir(std::io::Error),
    GitCloneStatus(std::process::ExitStatus),
    ScopeCollision(Span),
    ScopeItemMismatch(TypeId, TypeId),
    ScopeItemNotFound,
    ScopeItemCollision(ID),
}

parser::impl_error_display!((self, sources, { 
    modules: Modules, 
    units: Units,
    scope_item_lexicon: ScopeItemLexicon, 
}, f) => {
    match self {
        Kind::ModuleNotFound(path) => write!(f, "module not found: {:?}", path),
        Kind::ModuleLoadFailed(path, err) => write!(f, "module load failed: {:?}: {}", path, err),
        Kind::ModuleCycle(cycle) => {
            let modules = cycle
                .iter()
                .map(|&module| modules[module].source)
                .map(|source| format!("{}", sources[source].path.display()))
                .collect::<Vec<_>>()
                .join(" -> ");
            write!(f, "module cycle: {:?}", modules)
        }
        Kind::UnitNotFound(path) => write!(f, "unit not found: {:?}", path),
        Kind::UnitLoadFailed(path, err) => write!(f, "unit load failed: {:?}: {}", path, err),
        Kind::UnitCycle(cycle) => {
            let units = cycle
                .iter()
                .map(|&unit| units[unit].source)
                .map(|source| format!("{}", sources[source].path.display()))
                .collect::<Vec<_>>()
                .join(" -> ");
            write!(f, "unit cycle: {:?}", units)
        }
        Kind::SyntaxError(kind) => kind.print(sources, &(), f),
        Kind::GitClone(err) => write!(f, "git clone failed: {}", err),
        Kind::MkDir(err) => write!(f, "mkdir failed: {}", err),
        Kind::GitCloneStatus(status) => write!(f, "git clone failed: {}", status),
        Kind::ScopeCollision(span) => write!(f, "scope collision: {:?}", span),
        Kind::ScopeItemMismatch(expected, found) => {
            let expected = scope_item_lexicon.get(expected).unwrap();
            let found = scope_item_lexicon.get(found).unwrap();
            write!(f, "scope item mismatch: expected {:?}, found {:?}", expected, found)
        },
        Kind::ScopeItemNotFound => write!(f, "scope item not found"),
        Kind::ScopeItemCollision(id) => write!(f, "scope item collision: {:?}", id),
    }?;
});

impl From<parser::error::Kind> for Kind {
    fn from(other: parser::error::Kind) -> Self {
        Kind::SyntaxError(other)
    }
}
