use crate::*;

use cranelift_codegen::isa::CallConv;
use typec_types::*;

struct TyErrorDisplay<'a> {
    sources: &'a Sources,
    types: &'a Types,
    ty_lists: &'a TyLists,
}

pub fn display(
    error: &TyError,
    sources: &Sources,
    types: &Types,
    ty_lists: &TyLists,
    to: &mut String,
) -> std::fmt::Result {
    use std::fmt::Write;

    let state = TyErrorDisplay {
        sources,
        types,
        ty_lists,
    };

    match error {
        &TyError::NonPointerDereference { loc, ty } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "'{}' cannot be dereferenced", ty_display!(state, ty))
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
            tag.underline_info(sources, to, &|to| write!(to, "consider removing this"))?;
            generics.loc_to(sources, to)?;
            generics.underline_info(sources, to, &|to| write!(to, "or removing this"))?;
            writeln!(to, "|> If you still want the function to be generic, make a wrapper around concrete instance and mark it #entry")?;
        }
        &TyError::MissingBound { loc, input, bound } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "type '{}' does not implement bound '{}'",
                    ty_display!(state, input),
                    ty_display!(state, bound)
                )
            })?;
        }
        TyError::InfinitelySizedType { cycle } => {
            writeln!(to, "{ERR}|> infinitely sized type detected:{END}")?;
            for &ty in cycle {
                writeln!(to, "\t{}", ty_display!(state, ty))?;
            }
            writeln!(to, "|> Cycle can be broken by referencing any segment of the cycle indirectly.")?;
            writeln!(to, "|> Structure is depending indirectly if its hidden behind fixed sized interface like a pointer.")?;
        },
        TyError::InvalidCallConv { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "invalid calling convention")
            })?;
            writeln!(to, "|> valid calling conventions:")?;
            for cc in [
                // BRUH
                CallConv::Fast,
                CallConv::Cold,
                CallConv::SystemV,
                CallConv::WindowsFastcall,
                CallConv::AppleAarch64,
                CallConv::BaldrdashSystemV,
                CallConv::BaldrdashWindows,
                CallConv::Baldrdash2020,
                CallConv::Probestack,
                CallConv::WasmtimeSystemV,
                CallConv::WasmtimeFastcall,
                CallConv::WasmtimeAppleAarch64,
            ] {
                writeln!(to, "|\t{}", cc)?;
            }
        }
        &TyError::ReturnTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "expected '{}' but got '{}'",
                    ty_display!(state, expected),
                    ty_display!(state, got)
                )
            })?;

            if let Some(because) = because {
                because.loc_to(sources, to)?;
                because.underline_info(sources, to, &|to| {
                    write!(to, "because of this return type declaration")
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
        &TyError::BreakValueTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "'{}' is not consistent with", ty_display!(state, got))
            })?;

            because.loc_to(sources, to)?;
            because.underline_error(sources, to, &|to| {
                // write!(to, "'")?;
                // expected.display(types, sources, to)?;
                // write!(to, "' return value of this break")
                write!(
                    to,
                    "'{}' return value of this break",
                    ty_display!(state, expected)
                )
            })?;
        }
        &TyError::MissingBreakValue {
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
                write!(
                    to,
                    "because of '{}' return value in previous break",
                    ty_display!(state, expected)
                )
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
            because.underline_info(sources, to, &|to| write!(to, "this bound impl"))?;
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
            because.underline_info(sources, to, &|to| write!(to, "because of this definition"))?;
        }
        &TyError::CallArgTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "expected '{}' as a function argument but got '{}'",
                    ty_display!(state, expected),
                    ty_display!(state, got)
                )
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| write!(to, "because of this definition"))?;
        }
        TyError::UnknownField {
            candidates,
            on,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "unknown field on type '{}'", ty_display!(state, *on))
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
        &TyError::ExpectedStruct { got, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "expected struct type but got '{}'",
                    ty_display!(state, got)
                )
            })?;
        }
        &TyError::ConstructorFieldTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "expected '{}' which does not match",
                    ty_display!(state, got)
                )
            })?;

            because.loc_to(sources, to)?;
            because.underline_info(sources, to, &|to| {
                write!(to, "this field of type '{}'", ty_display!(state, expected))
            })?;
        }
        TyError::ConstructorMissingFields { on, missing, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "missing fields in constructor of '{}'",
                    ty_display!(state, *on)
                )
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
            because.underline_info(sources, to, &|to| write!(to, "because of this definition"))?;
            writeln!(
                to,
                "|> you can add a return type with ') -> <type> {{' syntax"
            )?;
        }
        &TyError::IfConditionTypeMismatch { got, loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "expected 'bool' type but got '{}'",
                    ty_display!(state, got)
                )
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
        &TyError::AssignTypeMismatch {
            because,
            expected,
            got,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "cannot assign '{}'", ty_display!(state, got))
            })?;

            because.underline_error(sources, to, &|to| {
                write!(to, "because this is '{}'", ty_display!(state, expected))
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
                ty_display!(state, left_ty),
                sources.display(loc),
                ty_display!(state, right_ty),
            )?;
        }
        TyError::InvalidPath { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "invalid path syntax"))?;
            writeln!(
                to,
                "|> possible syntaxes: '<module>::<item>' '<module>::<type>::<item>'"
            )?;
        }
        TyError::InvalidTypeExpression { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "invalid type expression"))?;
        }
        TyError::ExpectedConcreteType { loc } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| write!(to, "expected concrete type"))?;
        }
        &TyError::GenericTypeMismatch {
            expected,
            found,
            loc,
        } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(
                    to,
                    "expected type '{}' but got '{}'",
                    ty_display!(state, expected),
                    ty_display!(state, found)
                )
            })?;
            writeln!(to, "|> this happened during generic type inference")?;
        }
        TyError::UnknownGenericTypeParam { ty, loc, param } => {
            loc.loc_to(sources, to)?;
            loc.underline_error(sources, to, &|to| {
                write!(to, "unknown generic type parameter {}", param)
            })?;

            ty.loc_to(sources, to)?;
            ty.underline_info(sources, to, &|to| write!(to, "demanded by this type"))?;

            writeln!(
                to,
                "|> specify parameters explicitly: <type_name>::[<ty>, ..]"
            )?;
            writeln!(to, "|> use '_' instead of type for known parameters")?;
            writeln!(to, "|> omit tali parameters that are known")?;
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
