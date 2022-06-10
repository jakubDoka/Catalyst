use crate::*;
use lexer::*;

pub enum TyError {
    ExpectedCopyType {
        loc: Span,
    },
    CallArgCountMismatch {
        because: Result<Span, Ty>,
        expected: usize,
        got: usize,
        loc: Span,
    },
    CallNonFunction {
        ty: Ty,
        loc: Span,
    },
    ExplicitParamOverflow {
        because: Span,
        expected: usize,
        got: usize,
        loc: Span,
    },
    EnumVariantNotFound {
        ty: Ty,
        loc: Span,
    },
    FieldNotFound {
        ty: Ty,
        loc: Span,
    },
    ScopeItemNotFound {
        loc: Span,
    },
    ScopeCollision {
        items: Vec<Span>,
        loc: Span,
    },
    InvalidItemType {
        expected: String,
        got: String,
        loc: Span,
    },
    UnexpectedBoundFunc {
        bound: Ty,
        loc: Span,
    },
    UnregisteredFieldIndex {
        index: usize,
        max: usize,
        loc: Span,
        on: Ty,
    },
    InstantiationParamCountMismatch {
        expected: usize,
        got: usize,
        because: Span,
        loc: Span,
    },
    PatternTypeMismatch {
        expected: Ty,
        got: Ty,
        loc: Span,
        because: Span,
    },
    UnknownEnumVariant {
        loc: Span,
        on: Ty,
    },
    InfinitelySizedType {
        cycle: Vec<Ty>,
    },
    InvalidCallConv {
        loc: Span,
    },
    UnknownGenericTypeParam {
        ty: Span,
        loc: Span,
        param: usize,
    },
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
