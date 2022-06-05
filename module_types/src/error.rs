use crate::units::*;
use lexer::*;
use std::path::PathBuf;

#[derive(Debug)]
pub enum ModuleError {
    ScopeCollision {
        new: Span,
        existing: Span,
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
        path: PathBuf,
        trace: std::io::Error,
        loc: Span,
    },
    ModuleCycle {
        cycle: Vec<Source>,
    },
    RootUnitNotFound {
        path: PathBuf,
        trace: std::io::Error,
    },
    ManifestLoadFail {
        path: PathBuf,
        trace: std::io::Error,
        loc: Option<Span>,
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
