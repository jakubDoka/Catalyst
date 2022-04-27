use std::{any::TypeId, path::PathBuf};

use errors::{Palette, write_styled};
use lexer::{Span, Sources, SourcesExt, Source};

use crate::{Unit, ItemLexicon, Units};

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

impl Error {
    pub fn display(&self, sources: &Sources, scope_item_lexicon: &ItemLexicon, units: &Units, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            Error::ScopeCollision { new, existing } => {
                new.loc_to(sources, to)?;
                new.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to,
                    &|to| write!(to, "item with this identifier already exists"),
                )?;
                existing.underline_to(
                    Palette::info().bold(), 
                    '~', 
                    sources,
                    to,
                    &|to| write!(to, "identifier is used here"),
                )?;
            },
            Error::AmbiguousScopeItem { loc, suggestions } =>  {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(to, "ambiguous scope item, multiple items with this identifier are imported"),
                )?;
                writeln!(to, "|> specify the module with syntax '<module>::<ident>'")?;
                write!(to, "|> possible options:")?;
                for &suggestion in suggestions {
                    write!(to, " {}::{}", sources.display(suggestion), sources.display(*loc))?;
                }
                writeln!(to)?;
            },
            &Error::InvalidScopeItem { loc, expected, found } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "invalid scope item, expected {} but found {}", 
                        scope_item_lexicon.name_of(expected), 
                        scope_item_lexicon.name_of(found),
                    ),
                )?;
            },
            Error::ScopeItemNotFound { loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "scope item not found, will find a way to suggest... one day",
                    )
                )?;
            },
            Error::RootModuleNotFound { unit, trace } => {
                let unit = &units[*unit];
                let path = unit.root_path.clone();
                let source_path = unit.get_absolute_source_path().unwrap();
                write_styled!(to, Palette::error().bold(), "|> root module not found for unit '{}'", path.display());
                writeln!(to, "|> path searched: {}", source_path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::ModuleLoadFail { path, trace, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "module could not be loaded",
                    ),
                )?;
                writeln!(to, "|> path searched: {}", path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::ModuleNotFound { trace, path, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "module not found",
                    ),
                )?;
                writeln!(to, "|> path searched: {}", path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::ModuleCycle { cycle } => {
                write_styled!(to, Palette::error().bold(), "|> module cycle detected:\n",);
                for &module in cycle {
                    writeln!(to, "|\t{}", sources[module].path.display())?;
                }
                writeln!(to, "|> meta-programing features rely on this restriction")?;
            },
            Error::RootUnitNotFound { path, trace } => {
                write_styled!(to, Palette::error().bold(), "|> root of project not found",);
                writeln!(to, "|> path searched: {}", path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::ManifestLoadFail { path, trace, loc } => {
                if let Some(loc) = loc {
                    loc.loc_to(sources, to)?;
                    loc.underline_to(
                        Palette::error().bold(), 
                        '^', 
                        sources, 
                        to, 
                        &|to| write!(
                            to, 
                            "manifest of this import could not be loaded",
                        ),
                    )?;
                } else {
                    write_styled!(to, Palette::error().bold(), "|> root manifest could not be loaded",);
                }
                writeln!(to, "|> path searched: {}", path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::UnitNotFound { path, trace, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "unit not found",
                    ),
                )?;
                writeln!(to, "|> path searched: {}", path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::UnitCycle { cycle } => {
                write_styled!(to, Palette::error().bold(), "|> unit cycle detected:\n",);
                for &unit in cycle {
                    writeln!(to, "|\t{}", units[unit].root_path.display())?;
                }
                writeln!(to, "|> it's not realistic to allow cycles")?;
            },
            Error::MkGirtDir { path, trace, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "could not create girt directory",
                    ),
                )?;
                writeln!(to, "|> while attempting to create path: {}", path.display())?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::GitCloneExec { trace, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "git clone of this repo failed to execute",
                    ),
                )?;
                writeln!(to, "|> make sure you have git installed")?;
                writeln!(to, "|> backtrace: {}", trace)?;
            },
            Error::GitCloneStatus { code, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to, 
                    &|to| write!(
                        to, 
                        "git clone of this repo failed with status code {}",
                        code,
                    ),
                )?;
                writeln!(to, "|> check if you did not make a typo inside the link")?;
                writeln!(to, "|> syntax for git links is 'github.com/<user>/<repo>[@tag]' ([...] is optional)")?;
            },
        }

        writeln!(to)?;

        Ok(())
    }
}