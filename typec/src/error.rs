use errors::{write_styled, Palette};
use lexer::*;

use crate::*;

pub enum Error {
    UnknownGenericParam {
        loc: Span,
        func: Span,
        param: usize,
    },
    MissingBound {
        loc: Span,
        input: Ty,
        bound: Ty,
    },
    BoundImplFuncParamCount {
        impl_func: Span,
        bound_func: Span,
        expected: usize,
        found: usize,
    },
    MissingBoundImplFunc {
        func: Span,
        loc: Span,
    },
    DuplicateBoundImpl {
        because: Span,
        loc: Span,
    },
    DuplicateBound {
        loc: Span,
    },
    NonPointerDereference {
        loc: Span,
        ty: Ty,
    },
    GenericEntry {
        tag: Span,
        generics: Span,
        loc: Span,
    },
    InvalidPath {
        loc: Span,
    },
    BinaryOperatorNotFound {
        left_ty: Ty,
        right_ty: Ty,
        loc: Span,
    },
    ReturnTypeMismatch {
        because: Option<Span>,
        expected: Ty,
        got: Ty,
        loc: Span,
    },
    BreakValueTypeMismatch {
        because: Span,
        expected: Ty,
        got: Ty,
        loc: Span,
    },
    MissingBreakValue {
        because: Span,
        expected: Ty,
        loc: Span,
    },
    FunctionParamMismatch {
        because: Span,
        expected: usize,
        got: usize,
        loc: Span,
    },
    CallArgTypeMismatch {
        because: Span,
        expected: Ty,
        got: Ty,
        loc: Span,
    },
    UnknownField {
        candidates: Vec<Span>,
        on: Ty,
        loc: Span,
    },
    ExpectedStruct {
        got: Ty,
        loc: Span,
    },
    ConstructorFieldTypeMismatch {
        because: Span,
        expected: Ty,
        got: Ty,
        loc: Span,
    },
    ConstructorMissingFields {
        on: Ty,
        missing: Vec<Span>,
        loc: Span,
    },
    UnexpectedReturnValue {
        because: Span,
        loc: Span,
    },
    IfConditionTypeMismatch {
        got: Ty,
        loc: Span,
    },
    OperatorArgCountMismatch {
        because: Span,
        expected: usize,
        got: usize,
        loc: Span,
    },
    BinaryTypeMismatch {
        expected: Ty,
        got: Ty,
        loc: Span,
    },
    AssignToNonAssignable {
        because: Option<Span>,
        loc: Span,
    },
    AssignTypeMismatch {
        because: Span,
        expected: Ty,
        got: Ty,
        loc: Span,
    },
}

impl Error {
    pub fn display(&self, sources: &Sources, types: &Types, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            Error::NonPointerDereference { loc, ty } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "'")?;
                    ty.display(types, sources, to)?;
                    write!(to, "' cannot be dereferenced")
                })?;
                writeln!(to, "|> types that can be dereferenced: &<type> *<type>")?;
            }
            Error::DuplicateBound { loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "this generic parameter contains duplicate bound")
                })?;
            }
            Error::GenericEntry { tag, generics, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "function cannot be both entry point and generic")
                })?;
                tag.loc_to(sources, to)?;
                tag.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "consider removing this")
                })?;
                generics.loc_to(sources, to)?;
                generics.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "or removing this")
                })?;
                writeln!(to, "|> If you still want the function to be generic, make a wrapper around concrete instance and mark it #entry")?;
            }
            Error::MissingBound { loc, input, bound } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "type '")?;
                    input.display(types, sources, to)?;
                    write!(to, "' does not implement bound '")?;
                    bound.display(types, sources, to)?;
                    write!(to, "'")
                })?;
            }
            Error::ReturnTypeMismatch {
                because,
                expected,
                got,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected '")?;
                    expected.display(types, sources, to)?;
                    write!(to, "' but got '")?;
                    got.display(types, sources, to)?;
                    write!(to, "'")
                })?;

                if let Some(because) = because {
                    because.loc_to(sources, to)?;
                    because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                        write!(to, "because of this return typ declaration")
                    })?;
                }
            }
            Error::BoundImplFuncParamCount {
                impl_func,
                bound_func,
                expected,
                found,
            } => {
                impl_func.loc_to(sources, to)?;
                impl_func.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected {} parameter(s) but got {}", expected, found)
                })?;
                bound_func.loc_to(sources, to)?;
                bound_func.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "expected because of this function")
                })?;
            }
            Error::UnknownGenericParam { loc, func, param } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "unknown generic parameter {}", param)
                })?;

                func.loc_to(sources, to)?;
                func.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "function demanding the parameter")
                })?;

                writeln!(
                    to,
                    "|> specify parameters explicitly: <function_name>::[<ty>, ..]"
                )?;
                writeln!(to, "|> use '_' instead of type for known parameters")?;
                writeln!(to, "|> omit tali parameters that are known")?;
            }
            Error::BreakValueTypeMismatch {
                because,
                expected,
                got,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "'")?;
                    got.display(types, sources, to)?;
                    write!(to, "' is not consistent with")
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "'")?;
                    expected.display(types, sources, to)?;
                    write!(to, "' return value of this break")
                })?;
            }
            Error::MissingBreakValue {
                because,
                expected,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected this break to have value")
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "because of '")?;
                    expected.display(types, sources, to)?;
                    write!(to, "' return value in previous break")
                })?;
            }
            &Error::MissingBoundImplFunc { func, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "missing implementation of '{}'", sources.display(func))
                })?;
            }
            Error::DuplicateBoundImpl { because, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "this bound impl is colliding with")
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "this bound impl")
                })?;
            }
            Error::FunctionParamMismatch {
                because,
                expected,
                got,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected ")?;
                    write!(to, "{}", expected)?;
                    write!(to, " arguments but got ")?;
                    write!(to, "{}", got)
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "because of this definition")
                })?;
            }
            Error::CallArgTypeMismatch {
                because,
                expected,
                got,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected '")?;
                    expected.display(types, sources, to)?;
                    write!(to, "' as a function argument but got '")?;
                    got.display(types, sources, to)?;
                    write!(to, "'")
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "because of this definition")
                })?;
            }
            Error::UnknownField {
                candidates,
                on,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
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
            Error::ExpectedStruct { got, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected struct type but got '")?;
                    got.display(types, sources, to)?;
                    write!(to, "'")
                })?;
            }
            Error::ConstructorFieldTypeMismatch {
                because,
                expected,
                got,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "got '")?;
                    got.display(types, sources, to)?;
                    write!(to, "' which does not match")
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "this field of type '")?;
                    expected.display(types, sources, to)?;
                    write!(to, "'")
                })?;
            }
            Error::ConstructorMissingFields { on, missing, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
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
            Error::UnexpectedReturnValue { because, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "return value is not expected")
                })?;

                because.loc_to(sources, to)?;
                because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                    write!(to, "because of this definition")
                })?;
                writeln!(
                    to,
                    "|> you can add a return type with ') -> <type> {{' syntax"
                )?;
            }
            Error::IfConditionTypeMismatch { got, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "expected 'bool' type but got '")?;
                    got.display(types, sources, to)?;
                    write!(to, "'")
                })?;
                writeln!(to, "|> if can only take 'bool' as condition\n")?;
            }
            Error::AssignToNonAssignable { because, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "cannot assign to non-assignable type")
                })?;

                if let Some(because) = because {
                    because.loc_to(sources, to)?;
                    because.underline_to(Palette::info().bold(), '~', sources, to, &|to| {
                        write!(to, "because this value is declared immutable")
                    })?;

                    writeln!(
                        to,
                        "|> you can make it mutable with 'let mut <name> = <value>' syntax\n"
                    )?;
                }
            }
            Error::AssignTypeMismatch {
                because,
                expected,
                got,
                loc,
            } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "cannot assign '")?;
                    got.display(types, sources, to)?;
                    write!(to, "'")
                })?;

                because.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "because this is '")?;
                    expected.display(types, sources, to)?;
                    write!(to, "'")
                })?;
            }
            &Error::BinaryOperatorNotFound {
                left_ty,
                right_ty,
                loc,
            } => {
                write_styled!(
                    to,
                    Palette::error().bold(),
                    "|> Binary operator '{}' {} '{}' does not exist.",
                    ty::Display::new(types, sources, left_ty),
                    sources.display(loc),
                    ty::Display::new(types, sources, right_ty)
                )
            }
            Error::InvalidPath { loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(Palette::error().bold(), '^', sources, to, &|to| {
                    write!(to, "invalid path syntax")
                })?;
                writeln!(
                    to,
                    "|> possible syntaxes: '<module>::<item>' '<module>::<type>::<item>'"
                )?;
            }
            #[allow(unused)]
            Error::OperatorArgCountMismatch {
                because,
                expected,
                got,
                loc,
            } => todo!(),
            #[allow(unused)]
            Error::BinaryTypeMismatch { expected, got, loc } => todo!(),
        }

        writeln!(to)?;

        Ok(())
    }
}
