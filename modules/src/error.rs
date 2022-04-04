use std::{any::TypeId, path::PathBuf};

use lexer::{map::ID, prelude::Span};
use parser::prelude::AnyError;

use crate::{module::Module, unit::Unit};

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

impl From<parser::error::Kind> for Kind {
    fn from(other: parser::error::Kind) -> Self {
        Kind::SyntaxError(other)
    }
}
