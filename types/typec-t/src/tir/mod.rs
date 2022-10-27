use lexing_t::*;
use storage::*;

use crate::*;

pub type TypecOutput<A, T> = Vec<(A, VRef<T>)>;

#[derive(Default)]
pub struct TirBuilderCtx {
    pub vars: Vec<VarHeaderTir>,
    pub generics: Vec<VSlice<Spec>>,
    pub cast_checks: Vec<CastCheck>,
    pub macros: Vec<MacroCompileRequest>,
}

#[derive(Clone, Copy)]
pub struct MacroCompileRequest {
    pub name: VRef<str>,
    pub ty: Ty,
    pub r#impl: VRef<Impl>,
}

pub struct CastCheck {
    pub loc: Span,
    pub from: Ty,
    pub to: Ty,
}

pub struct TirBuilder<'arena, 'ctx> {
    pub arena: &'arena Arena,
    pub ret: Ty,
    pub ret_span: Option<Span>,
    pub runner: Option<(Span, TirFrame)>,
    pub ctx: &'ctx mut TirBuilderCtx,
}

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub fn new(
        arena: &'arena Arena,
        ret: Ty,
        ret_span: Option<Span>,
        ctx: &'ctx mut TirBuilderCtx,
    ) -> Self {
        Self {
            arena,
            ret,
            ret_span,
            runner: None,
            ctx,
        }
    }

    pub fn start_frame(&mut self) -> TirFrame {
        TirFrame(self.ctx.vars.len())
    }

    pub fn end_frame(&mut self, frame: TirFrame) {
        self.ctx.vars.truncate(frame.0);
    }

    pub fn create_var(&mut self, mutable: bool, ty: Ty, span: Span) -> VRef<VarHeaderTir> {
        let index = self.ctx.vars.len();
        self.ctx.vars.push(VarHeaderTir { ty, span, mutable });
        unsafe { VRef::new(index) }
    }

    pub fn get_var(&self, var: VRef<VarHeaderTir>) -> VarHeaderTir {
        self.ctx.vars[var.index()]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VarHeaderTir {
    pub ty: Ty,
    pub span: Span,
    pub mutable: bool,
}

#[must_use]
pub struct TirFrame(usize);

impl TirFrame {
    pub fn contains(&self, var: VRef<VarHeaderTir>) -> bool {
        var.index() >= self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CallTir<'a> {
    pub func: CallableTir<'a>,
    pub params: &'a [Ty],
    pub args: &'a [TirNode<'a>],
}

#[derive(Clone, Copy, Debug)]
pub enum CallableTir<'a> {
    Func(VRef<Func>),
    SpecFunc(VRef<SpecFunc>),
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
    Struct { fields: &'a [PatTir<'a>] },
    Binding(bool, VRef<VarHeaderTir>),
    Int(Result<Span, i64>, VRef<Func>),
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
    pub span: Span,
}

impl<'a> TirNode<'a> {
    pub fn new(ty: Ty, kind: TirKind<'a>, span: Span) -> Self {
        Self { kind, ty, span }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TirKind<'a> {
    Int(Option<i64>),
    Char,
    Bool(bool),
    Block(&'a [TirNode<'a>]),
    Return(Option<&'a TirNode<'a>>),
    Call(&'a CallTir<'a>),
    Access(VRef<VarHeaderTir>),
    Const(&'a TirNode<'a>),
    Ctor(&'a [TirNode<'a>]),
    Deref(&'a TirNode<'a>),
    Ref(&'a TirNode<'a>),
    Match(&'a MatchTir<'a>),
    If(&'a IfTir<'a>),
    Field(&'a FieldTir<'a>),
    Let(&'a LetTir<'a>),
    Assign(&'a AssignTir<'a>),
}
