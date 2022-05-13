use instance_types::*;
use lexer_types::*;
use storage::ReservedValue;
use typec_types::*;

struct State<'a> {
    types: &'a Types,
    ty_lists: &'a TyLists,
    sources: &'a Sources,
}

pub fn display(
    err: &InstError,
    types: &Types,
    ty_lists: &TyLists,
    sources: &Sources,
    to: &mut String,
) -> std::fmt::Result {
    use std::fmt::Write;

    let state = State {
        types,
        ty_lists,
        sources,
    };

    let _ = ty_display!(state, Ty::reserved_value());

    match err {
        InstError::InvalidBitCast {
            loc,
            instantiated_from,
            from,
            from_size,
            to: to_ty,
            to_size,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "invalid bit cast form '{}'({}) to '{}'({}) as size does not match",
                    from, from_size, to_ty, to_size,
                )
            })?;

            if let Some(instantiated_from) = instantiated_from {
                instantiated_from.loc_to(sources, to)?;
                instantiated_from
                    .underline_info(sources, to, &|to| write!(to, "instantiated from here"))?;
            }
        }
    }

    Ok(())
}
