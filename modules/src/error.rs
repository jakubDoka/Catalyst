use lexer::*;
use module_types::{error::ModuleError, scope::ItemLexicon, units::Units};
use storage::*;

pub fn display(
    error: &ModuleError,
    sources: &Sources,
    scope_item_lexicon: &ItemLexicon,
    units: &Units,
    to: &mut String,
) -> std::fmt::Result {
    use std::fmt::Write;
    match error {
        ModuleError::ScopeCollision { new, existing } => {
            new.loc_to(sources, to)?;
            new.underline_error(sources, to, &|to| {
                write!(to, "item with this identifier already exists")
            })?;
            existing.underline_info(sources, to, &|to| write!(to, "identifier is used here"))?;
        }
        ModuleError::AmbiguousScopeItem { loc, suggestions } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "ambiguous scope item, multiple items with this identifier are imported"
                )
            })?;
            writeln!(to, "|> specify the module with syntax '<module>::<ident>'")?;
            write!(to, "|> possible options:")?;
            for &suggestion in suggestions {
                write!(
                    to,
                    " {}::{}",
                    sources.display(suggestion),
                    sources.display(*loc)
                )?;
            }
            writeln!(to)?;
        }
        &ModuleError::InvalidScopeItem {
            loc,
            ref expected,
            found,
        } => {
            let expected = expected
                .iter()
                .map(|&exp| 
                    scope_item_lexicon.name_of(exp)    
                )
                .collect::<Vec<_>>()
                .join(" | ");

            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "invalid scope item, expected {} but found {}",
                    expected,
                    scope_item_lexicon.name_of(found),
                )
            })?;
        }
        ModuleError::ScopeItemNotFound { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "scope item not found, will find a way to suggest... one day",
                )
            })?;
        }
        ModuleError::RootModuleNotFound { unit, trace } => {
            let unit = &units[*unit];
            let path = unit.root_path.clone();
            let source_path = path.join(&unit.local_source_path);
            write_colored!(
                to,
                ansi_consts::ERR,
                "|> root module not found for unit '{}'\n",
                path.display()
            )?;
            writeln!(to, "|> path searched: {}", source_path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::ModuleLoadFail { path, trace, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "module could not be loaded",))?;
            writeln!(to, "|> path searched: {}", path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::ModuleNotFound { trace, path, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "module not found",))?;
            writeln!(to, "|> path searched: {}", path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::ModuleCycle { cycle } => {
            write_colored!(to, ansi_consts::ERR, "|> module cycle detected:\n",)?;
            for &module in cycle {
                writeln!(to, "|\t{}", sources[module].path.display())?;
            }
            writeln!(to, "|> meta-programing features rely on this restriction")?;
        }
        ModuleError::RootUnitNotFound { path, trace } => {
            writeln!(to, "{ERR}|> root of project not found{END}")?;
            writeln!(to, "|> path searched: {}", path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::ManifestLoadFail { path, trace, loc } => {
            if let Some(loc) = loc {
                loc.loc_to(sources, to)?;
                loc.underline_error(sources, to, &|to| {
                    write!(to, "manifest of this import could not be loaded")
                })?;
            } else {
                writeln!(to, "{ERR}|> root manifest could not be loaded{END}")?;
            }
            writeln!(to, "|> path searched: {}", path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::UnitNotFound { path, trace, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "unit not found",))?;
            writeln!(to, "|> path searched: {}", path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::UnitCycle { cycle } => {
            write_colored!(to, ansi_consts::ERR, "|> unit cycle detected:\n",)?;
            for &unit in cycle {
                writeln!(to, "|\t{}", units[unit].root_path.display())?;
            }
            writeln!(to, "|> it's not realistic to allow cycles")?;
        }
        ModuleError::MkGirtDir { path, trace, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "could not create girt directory",)
            })?;
            writeln!(to, "|> while attempting to create path: {}", path.display())?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::GitCloneExec { trace, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "git clone of this repo failed to execute",)
            })?;
            writeln!(to, "|> make sure you have git installed")?;
            writeln!(to, "|> backtrace: {}", trace)?;
        }
        ModuleError::GitCloneStatus { code, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "git clone of this repo failed with status code {}",
                    code,
                )
            })?;
            writeln!(to, "|> check if you did not make a typo inside the link")?;
            writeln!(
                to,
                "|> syntax for git links is 'github.com/<user>/<repo>[@tag]' ([...] is optional)"
            )?;
        }
    }

    writeln!(to)?;

    Ok(())
}
