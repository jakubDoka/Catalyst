use std::{any::TypeId, path::PathBuf};

use lexer::Span;

use crate::{Unit, Module};

#[derive(Debug)]
pub enum Error {
    ScopeCollision {
        new: Span,
        existing: Span,
    },
    AmbiguousScopeItem {
        loc: Span,
        suggestions: Vec<Span>,
    },
    InvalidScopeItem {
        loc: Span,
        expected: TypeId,
        found: TypeId,
    },
    ScopeItemNotFound {
        loc: Span,
    },
    RootModuleNotFound {
        unit: Unit,
        trace: std::io::Error,
    },
    ModuleLoadFail {
        path: PathBuf,
        trace: std::io::Error,
        loc: Span,
    },
    ModuleNotFound {
        trace: std::io::Error,
        loc: Span,
    },
    ModuleCycle {
        cycle: Vec<Module>,
    },
    RootUnitNotFound {
        path: PathBuf,
        trace: std::io::Error,
    },
    ManifestLoadFail {
        path: PathBuf,
        trace: std::io::Error,
        loc: Span,
    },
    UnitNotFound {
        path: PathBuf,
        trace: std::io::Error,
        loc: Span,
    },
    UnitCycle {
        cycle: Vec<Unit>,
    },
    MkGirtDir {
        path: PathBuf,
        trace: std::io::Error,
        loc: Span,
    },

    GitCloneExec {
        trace: std::io::Error,
        loc: Span,
    },

    GitCloneStatus {
        code: std::process::ExitStatus,
        loc: Span,
    },
}