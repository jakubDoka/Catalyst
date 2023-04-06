use std::default::default;

use span::*;
use storage::*;

use crate::*;
use rkyv::{Archive, Deserialize, Serialize};

pub type TypecOutput<A, T> = Vec<(A, FragRef<T>)>;

pub struct LoopHeaderTir<'a> {
    pub return_type: Ty<'a>,
    pub inference: Inference<'a>,
    pub label: Option<Ident>,
}

#[derive(Clone, Copy)]
pub struct MacroCompileRequest {
    pub name: Ident,
    pub ty: CompactTy,
    pub r#impl: FragRef<Impl>,
    pub params: FragSlice<CompactTy>,
}

pub struct CastCheck {
    pub loc: Span,
    pub from: CompactTy,
    pub to: CompactTy,
}

#[derive(Clone, Copy, Debug)]
pub struct VarHeaderTir<'a> {
    pub ty: Ty<'a>,
    pub span: Span,
    pub mutable: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct CallTir<'a> {
    pub func: CallableTir<'a>,
    pub params: &'a [Ty<'a>],
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
    pub ty: Ty<'a>,
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
    Binding(bool, VRef<VarHeaderTir<'a>>),
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
    pub ty: Ty<'a>,
    pub flags: TirFlags,
    pub span: Span,
}

impl<'a> TirNode<'a> {
    #[inline]
    pub fn new(ty: Ty<'a>, kind: TirKind<'a>, span: Span) -> Self {
        Self {
            kind,
            ty,
            flags: default(),
            span,
        }
    }

    #[inline]
    pub fn with_flags(ty: Ty<'a>, kind: TirKind<'a>, flags: TirFlags, span: Span) -> Self {
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
    pub id: VRef<LoopHeaderTir<'a>>,
    pub body: TirNode<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct BreakTir<'a> {
    pub loop_id: VRef<LoopHeaderTir<'a>>,
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
    Access(VRef<VarHeaderTir<'a>>),
    Ctor(&'a [TirNode<'a>]),
    Deref(&'a TirNode<'a>),
    Ref(&'a TirNode<'a>),
    Match(&'a MatchTir<'a>),
    If(&'a IfTir<'a>),
    Field(&'a FieldTir<'a>),
    Let(&'a LetTir<'a>),
    Assign(&'a AssignTir<'a>),
    Loop(&'a LoopTir<'a>),
    Continue(VRef<LoopHeaderTir<'a>>),
    Break(&'a BreakTir<'a>),
}

#[derive(Clone, Copy)]
pub enum Inference<'a> {
    Strong(Ty<'a>),
    Weak(Ty<'a>),
    None,
}

impl<'a> Inference<'a> {
    pub fn ty(self) -> Option<Ty<'a>> {
        match self {
            Self::Strong(ty) | Self::Weak(ty) => Some(ty),
            Self::None => None,
        }
    }

    pub fn weaken(self) -> Self {
        match self {
            Self::Strong(ty) => Self::Weak(ty),
            other => other,
        }
    }

    pub fn map(self, f: impl FnOnce(Ty) -> Option<Ty>) -> Self {
        match self {
            Self::Strong(ty) => f(ty).map(Self::Strong).unwrap_or(Self::None),
            Self::Weak(ty) => f(ty).map(Self::Weak).unwrap_or(Self::None),
            Self::None => Self::None,
        }
    }
}

impl<'a> From<Option<Ty<'a>>> for Inference<'a> {
    fn from(ty: Option<Ty<'a>>) -> Self {
        match ty {
            Some(ty) => Self::Strong(ty),
            None => Self::None,
        }
    }
}
