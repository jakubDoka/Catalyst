use crate::*;

use typec_types::*;

pub fn display(error: &TyError, sources: &Sources, types: &Types, to: &mut String) -> std::fmt::Result {
    use std::fmt::Write;
    match error {
        TyError::NonPointerDereference { loc, ty } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "'")?;
                ty.display(types, sources, to)?;
                write!(to, "' cannot be dereferenced")
            })?;
            writeln!(to, "|> types that can be dereferenced: &<type> *<type>")?;
        }
        TyError::DuplicateBound { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "this generic parameter contains duplicate bound")
            })?;
        }
        TyError::GenericEntry { tag, generics, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "function cannot be both entry point and generic")
            })?;
            tag.loc_to(sources, to)?;
            tag.underline_info(sources, to, &|to| {
                write!(to, "consider removing this")
            })?;
            generics.loc_to(sources, to)?;
            generics.underline_info(sources, to, &|to| {
                write!(to, "or removing this")
            })?;
            writeln!(to, "|> If you still want the function to be generic, make a wrapper around concrete instance and mark it #entry")?;
        }
        TyError::MissingBound { loc, input, bound } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "type '")?;
                input.display(types, sources, to)?;
                write!(to, "' does not implement bound '")?;
                bound.display(types, sources, to)?;
                write!(to, "'")
            })?;
        }
        TyError::ReturnTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected '")?;
                expected.display(types, sources, to)?;
                write!(to, "' but got '")?;
                got.display(types, sources, to)?;
                write!(to, "'")
            })?;

            if let Some(because) = because {
                because.loc_to(sources, to)?;
                because.underline_info(sources, to, &|to| {
                    write!(to, "because of this return typ declaration")
                })?;
            }
        }
        TyError::BoundImplFuncParamCount {
            impl_func,
            bound_func,
            expected,
            found,
        } => {
            impl_func.loc_to(sources, to)?;
            impl_func.underline_error(sources, to, &|to| {
                write!(to, "expected {} parameter(s) but got {}", expected, found)
            })?;
            bound_func.loc_to(sources, to)?;
            bound_func.underline_info(sources, to, &|to| {
                write!(to, "expected because of this function")
            })?;
        }
        TyError::UnknownGenericParam { loc, func, param } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "unknown generic parameter {}", param)
            })?;

            func.loc_to(sources, to)?;
            func.underline_info(sources, to, &|to| {
                write!(to, "function demanding the parameter")
            })?;

            writeln!(
                to,
                "|> specify parameters explicitly: <function_name>::[<ty>, ..]"
            )?;
            writeln!(to, "|> use '_' instead of type for known parameters")?;
            writeln!(to, "|> omit tali parameters that are known")?;
        }
        TyError::BreakValueTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "'")?;
                got.display(types, sources, to)?;
                write!(to, "' is not consistent with")
            })?;

            because.loc_to(sources, to)?;
            because.underline_error(sources, to, &|to| {
                write!(to, "'")?;
                expected.display(types, sources, to)?;
                write!(to, "' return value of this break")
            })?;
        }
        TyError::MissingBreakValue {
            because,
            expected,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected this break to have value")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because of '")?;
                expected.display(types, sources, to)?;
                write!(to, "' return value in previous break")
            })?;
        }
        &TyError::MissingBoundImplFunc { func, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "missing implementation of '{}'", sources.display(func))
            })?;
        }
        TyError::DuplicateBoundImpl { because, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "this bound impl is colliding with")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "this bound impl")
            })?;
        }
        TyError::FunctionParamMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected ")?;
                write!(to, "{}", expected)?;
                write!(to, " arguments but got ")?;
                write!(to, "{}", got)
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because of this definition")
            })?;
        }
        TyError::CallArgTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected '")?;
                expected.display(types, sources, to)?;
                write!(to, "' as a function argument but got '")?;
                got.display(types, sources, to)?;
                write!(to, "'")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because of this definition")
            })?;
        }
        TyError::UnknownField {
            candidates,
            on,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "unknown field on type '")?;
                on.display(types, sources, to)?;
                write!(to, "'")
            })?;

            if candidates.is_empty() {
                writeln!(to, "|> type has no fields\n")?;
            } else {
                write!(to, "|> possible fields:")?;
                for &candidate in candidates {
                    write!(to, " '{}'", sources.display(candidate))?;
                }
                writeln!(to)?;
            }
        }
        TyError::ExpectedStruct { got, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected struct type but got '")?;
                got.display(types, sources, to)?;
                write!(to, "'")
            })?;
        }
        TyError::ConstructorFieldTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "got '")?;
                got.display(types, sources, to)?;
                write!(to, "' which does not match")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "this field of type '")?;
                expected.display(types, sources, to)?;
                write!(to, "'")
            })?;
        }
        TyError::ConstructorMissingFields { on, missing, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "missing fields in constructor of '")?;
                on.display(types, sources, to)?;
                write!(to, "'")
            })?;

            write!(to, "|> missing fields:",)?;
            for &field in missing {
                write!(to, " '{}'", sources.display(field))?;
            }
            writeln!(to)?;
        }
        TyError::UnexpectedReturnValue { because, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "return value is not expected")
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "because of this definition")
            })?;
            writeln!(
                to,
                "|> you can add a return type with ') -> <type> {{' syntax"
            )?;
        }
        TyError::IfConditionTypeMismatch { got, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected 'bool' type but got '")?;
                got.display(types, sources, to)?;
                write!(to, "'")
            })?;
            writeln!(to, "|> if can only take 'bool' as condition\n")?;
        }
        TyError::AssignToNonAssignable { because, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "cannot assign to non-assignable type")
            })?;

            if let Some(because) = because {
                because.loc_to(sources, to)?;
                because.underline_info(sources, to, &|to| {
                    write!(to, "because this value is declared immutable")
                })?;

                writeln!(
                    to,
                    "|> you can make it mutable with 'let mut <name> = <value>' syntax\n"
                )?;
            }
        }
        TyError::AssignTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "cannot assign '")?;
                got.display(types, sources, to)?;
                write!(to, "'")
            })?;

            because.underline_error(sources, to, &|to| {
                write!(to, "because this is '")?;
                expected.display(types, sources, to)?;
                write!(to, "'")
            })?;
        }
        &TyError::BinaryOperatorNotFound {
            left_ty,
            right_ty,
            loc,
        } => {
            write_colored!(
                to,
                ansi_consts::ERR,
                "|> Binary operator '{}' {} '{}' does not exist.",
                TyDisplay::new(types, sources, left_ty),
                sources.display(loc),
                TyDisplay::new(types, sources, right_ty)
            )?;
        }
        TyError::InvalidPath { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "invalid path syntax")
            })?;
            writeln!(
                to,
                "|> possible syntaxes: '<module>::<item>' '<module>::<type>::<item>'"
            )?;
        }
        TyError::InvalidTypeExpression { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "invalid type expression")
            })?;
        }
        TyError::ExpectedConcreteType { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected concrete type")
            })?;
        }
        TyError::GenericTypeMismatch {
            expected,
            found,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "expected type '")?;
                expected.display(types, sources, to)?;
                write!(to, "' but got '")?;
                found.display(types, sources, to)?;
                write!(to, "'")
            })?;
        }
        #[allow(unused)]
        TyError::OperatorArgCountMismatch {
            because,
            expected,
            got,
            loc,
        } => todo!(),
        #[allow(unused)]
        TyError::BinaryTypeMismatch { expected, got, loc } => todo!(),
    }

    writeln!(to)?;

    Ok(())
}
