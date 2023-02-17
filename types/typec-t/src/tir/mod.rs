use std::default::default;

use lexing_t::*;
use storage::*;

use crate::*;
use rkyv::{Archive, Deserialize, Serialize};

pub type TypecOutput<A, T> = Vec<(A, FragRef<T>)>;

pub struct LoopHeaderTir {
    pub return_type: Ty,
    pub inference: Inference,
    pub label: Option<Ident>,
}

#[derive(Clone, Copy)]
pub struct MacroCompileRequest {
    pub name: Ident,
    pub ty: Ty,
    pub r#impl: FragRef<Impl>,
    pub params: FragSlice<Ty>,
}

pub struct CastCheck {
    pub loc: Span,
    pub from: Ty,
    pub to: Ty,
}

#[derive(Clone, Copy, Debug)]
pub struct VarHeaderTir {
    pub ty: Ty,
    pub span: Span,
    pub mutable: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct CallTir<'a> {
    pub func: CallableTir<'a>,
    pub params: &'a [Ty],
    pub args: &'a [TirNode<'a>],
}

#[derive(Clone, Copy, Debug)]
pub enum CallableTir<'a> {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>),
    Pointer(TirNode<'a>),
}

#[derive(Clone, Copy, Debug)]
pub struct MatchTir<'a> {
    pub value: TirNode<'a>,
    pub arms: &'a [MatchArmTir<'a>],
}

#[derive(Clone, Copy, Debug)]
pub struct MatchArmTir<'a> {
    pub pat: PatTir<'a>,
    pub body: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct PatTir<'a> {
    pub kind: PatKindTir<'a>,
    pub has_binding: bool,
    pub is_refutable: bool,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Clone, Copy, Debug)]
pub enum PatKindTir<'a> {
    Unit(UnitPatKindTir<'a>),
    Or(&'a [UnitPatKindTir<'a>]),
}

#[derive(Clone, Copy, Debug)]
pub enum UnitPatKindTir<'a> {
    Enum {
        id: u32,
        ty: FragRef<Enum>,
        value: Option<&'a PatTir<'a>>,
    },
    Struct {
        fields: &'a [PatTir<'a>],
    },
    Binding(bool, VRef<VarHeaderTir>),
    Int(Result<Span, i64>),
    Wildcard,
}

#[derive(Clone, Copy, Debug)]
pub struct FieldTir<'a> {
    pub field: u32,
    pub header: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct IfBranchTir<'a> {
    pub cond: TirNode<'a>,
    pub body: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct IfTir<'a> {
    pub top: IfBranchTir<'a>,
    pub elifs: &'a [IfBranchTir<'a>],
    pub r#else: Option<TirNode<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub struct LetTir<'a> {
    pub pat: PatTir<'a>,
    pub value: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct AssignTir<'a> {
    pub lhs: TirNode<'a>,
    pub rhs: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct TirNode<'a> {
    pub kind: TirKind<'a>,
    pub ty: Ty,
    pub flags: TirFlags,
    pub span: Span,
}

impl<'a> TirNode<'a> {
    #[inline]
    pub fn new(ty: Ty, kind: TirKind<'a>, span: Span) -> Self {
        Self {
            kind,
            ty,
            flags: default(),
            span,
        }
    }

    #[inline]
    pub fn with_flags(ty: Ty, kind: TirKind<'a>, flags: TirFlags, span: Span) -> Self {
        Self {
            kind,
            ty,
            flags,
            span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TirFunc<'a> {
    pub args: &'a [PatTir<'a>],
    pub body: TirNode<'a>,
}

bitflags! {
    TirFlags: u8 {
        IMMUTABLE
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LoopTir<'a> {
    pub id: VRef<LoopHeaderTir>,
    pub body: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct BreakTir<'a> {
    pub loop_id: VRef<LoopHeaderTir>,
    pub value: Option<TirNode<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub enum TirKind<'a> {
    Int(Option<i64>),
    Float(Option<f64>),
    Char,
    Bool(bool),
    Block(&'a [TirNode<'a>]),
    Return(Option<&'a TirNode<'a>>),
    Call(&'a CallTir<'a>),
    ConstAccess(FragRef<Const>),
    Access(VRef<VarHeaderTir>),
    Ctor(&'a [TirNode<'a>]),
    Deref(&'a TirNode<'a>),
    Ref(&'a TirNode<'a>),
    Match(&'a MatchTir<'a>),
    If(&'a IfTir<'a>),
    Field(&'a FieldTir<'a>),
    Let(&'a LetTir<'a>),
    Assign(&'a AssignTir<'a>),
    Loop(&'a LoopTir<'a>),
    Continue(VRef<LoopHeaderTir>),
    Break(&'a BreakTir<'a>),
}

#[derive(Clone, Copy)]
pub enum Inference {
    Strong(Ty),
    Weak(Ty),
    None,
}

impl Inference {
    pub fn ty(self) -> Option<Ty> {
        match self {
            Inference::Strong(ty) | Inference::Weak(ty) => Some(ty),
            Inference::None => None,
        }
    }

    pub fn weaken(self) -> Inference {
        match self {
            Inference::Strong(ty) => Inference::Weak(ty),
            other => other,
        }
    }
}

impl From<Option<Ty>> for Inference {
    fn from(ty: Option<Ty>) -> Self {
        match ty {
            Some(ty) => Inference::Strong(ty),
            None => Inference::None,
        }
    }
}
