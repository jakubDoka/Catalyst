use lexing_t::*;
use storage::*;

use crate::*;

pub type TypecOutput<A, T> = Vec<(A, VRef<T>)>;

pub struct TirBuilder<'a> {
    pub arena: &'a Arena,
    pub ret: Ty,
    pub ret_span: Option<Span>,
    pub vars: Vec<VarTir<'a>>,
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

    pub fn create_var(&mut self, node: TirKind<'a>, ty: Ty, span: Span) -> VRef<Var> {
        let index = self.vars.len();
        self.vars.push(VarTir { node, ty, span });
        unsafe { VRef::new(index) }
    }

    pub fn next_var(&self) -> VRef<Var> {
        unsafe { VRef::new(self.vars.len()) }
    }

    pub fn get_var(&self, var: VRef<Var>) -> VarTir<'a> {
        self.vars[var.index()]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VarTir<'a> {
    pub node: TirKind<'a>,
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

pub struct Var;

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
    Constructor(&'a [TirNode<'a>]),
    Deref(&'a TirNode<'a>),
    Ref(&'a TirNode<'a>),
}
