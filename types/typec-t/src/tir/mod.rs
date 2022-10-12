use lexing_t::*;
use storage::*;

use crate::*;

pub type TypecOutput<A, T> = Vec<(A, VRef<T>)>;

pub struct TirBuilder<'a> {
    pub arena: &'a Arena,
    pub ret: Ty,
    pub ret_span: Option<Span>,
    pub vars: Vec<Var>,
    pub generics: Vec<VSlice<Spec>>,
    pub runner: Option<(Span, TirFrame)>,
}

impl<'a> TirBuilder<'a> {
    pub fn new(
        arena: &'a Arena,
        ret: Ty,
        ret_span: Option<Span>,
        generics: Vec<VSlice<Spec>>,
    ) -> Self {
        Self {
            arena,
            ret,
            ret_span,
            vars: Vec::new(),
            generics,
            runner: None,
        }
    }

    pub fn start_frame(&mut self) -> TirFrame {
        TirFrame(self.vars.len())
    }

    pub fn end_frame(&mut self, frame: TirFrame) {
        self.vars.truncate(frame.0);
    }

    pub fn create_var(&mut self, ty: Ty, span: Span) -> VRef<Var> {
        let index = self.vars.len();
        self.vars.push(Var { ty, span });
        unsafe { VRef::new(index) }
    }

    pub fn get_var(&self, var: VRef<Var>) -> Var {
        self.vars[var.index()]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Var {
    pub ty: Ty,
    pub span: Span,
}

#[must_use]
pub struct TirFrame(usize);

impl TirFrame {
    pub fn contains(&self, var: VRef<Var>) -> bool {
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
    Binding(VRef<Var>),
    Int(Span, VRef<Func>),
    Wildcard,
}

#[derive(Clone, Copy, Debug)]
pub struct FieldTir<'a> {
    pub field: u32,
    pub header: TirNode<'a>,
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
    Var(Option<&'a TirNode<'a>>),
    Int,
    Char,
    Block(&'a [TirNode<'a>]),
    Return(Option<&'a TirNode<'a>>),
    Call(&'a CallTir<'a>),
    Access(VRef<Var>),
    Const(&'a TirNode<'a>),
    Ctor(&'a [TirNode<'a>]),
    Deref(&'a TirNode<'a>),
    Ref(&'a TirNode<'a>),
    Match(&'a MatchTir<'a>),
    Field(&'a FieldTir<'a>),
}
