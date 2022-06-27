use lexer::*;
use typec_types::*;

pub enum OwError {
    DoubleMove { ty: Ty, because: Span, loc: Span },
    MoveFromBehindPointer { ty: Ty, loc: Span },
    LoopDoubleMove { ty: Ty, because: Span, loc: Span },
    PartiallyMovedDrop { ty: Ty, because: Span, loc: Span },
}

pub fn display(error: &OwError, ty_lists: &TyLists, ty_comps: &TyComps, types: &Types, sources: &Sources, to: &mut String) -> std::fmt::Result {
    use std::fmt::Write;

    struct State<'a> {
        ty_lists: &'a TyLists,
        ty_comps: &'a TyComps,
        types: &'a Types,
        sources: &'a Sources,
    }

    let state = State {
        ty_lists,
        ty_comps,
        types,
        sources,
    };

    let &ty = match error {
        OwError::DoubleMove { ty, loc, because } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "accessing already moved memory")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because of this previous move")
            })?;
            ty
        }
        OwError::MoveFromBehindPointer { ty, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "move from behind a pointer"))?;
            ty
        }
        OwError::LoopDoubleMove { ty, because, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "accessing already moved memory")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because this value is outside of loop")
            })?;
            ty
        }
        OwError::PartiallyMovedDrop { ty, loc, because } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "detected partially moved value that implements 'drop' bound when generating drops here")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| write!(to, "caused by this move"))?;
            ty
        }
    };
    writeln!(to, "|> '{}' does not implement 'copy' bound", ty_display!(state, ty))?;
    Ok(())
}
