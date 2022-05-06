use lexer_types::*;
use crate::*;

pub enum TyError {
    InvalidTypeExpression {
        loc: Span,
    },
    ExpectedConcreteType {
        loc: Span,
    },
    GenericTypeMismatch {
        expected: Ty,
        found: Ty,
        loc: Span,
    },
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