use lexer::*;

pub enum OwError {
    DoubleMove { because: Span, loc: Span },
    MoveFromBehindPointer { loc: Span },
    LoopDoubleMove { because: Span, loc: Span },
    PartiallyMovedDrop { because: Span, loc: Span },
}

pub fn display(error: &OwError, sources: &Sources, to: &mut String) -> std::fmt::Result {
    use std::fmt::Write;
    match error {
        OwError::DoubleMove { loc, because } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "accessing already moved memory")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because of this previous move")
            })?;
        }
        OwError::MoveFromBehindPointer { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "move behind a pointer"))?;
        }
        OwError::LoopDoubleMove { because, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "accessing already moved memory")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because this value is outside of loop")
            })?;
        }
        OwError::PartiallyMovedDrop { loc, because } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "detected partially moved value that implements 'drop' bound when generating drops here")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| write!(to, "caused by this move"))?;
        }
    }
    writeln!(to, "|> type does not implement 'copy' bound")?;
    Ok(())
}
