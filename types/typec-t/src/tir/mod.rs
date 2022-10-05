use lexing_t::*;
use storage::*;

use crate::{Func, SpecFunc, Ty};

pub type TypecOutput<T> = Vec<(usize, VRef<T>)>;

pub struct TirBuilder<'a> {
    pub arena: &'a Arena,
    pub ret: VRef<Ty>,
    pub ret_span: Option<Span>,
    pub vars: Vec<VarTir<'a>>,
    pub generics: Vec<VRef<Ty>>,
    pub runner: Option<(Span, TirFrame)>,
}

impl<'a> TirBuilder<'a> {
    pub fn new(
        arena: &'a Arena,
        ret: VRef<Ty>,
        ret_span: Option<Span>,
        generics: Vec<VRef<Ty>>,
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

    pub fn node<T: NodeInput<'a>>(&mut self, node: T) -> TirNode<'a> {
        node.convert(self.arena)
    }

    pub fn start_frame(&mut self) -> TirFrame {
        TirFrame(self.vars.len())
    }

    pub fn end_frame(&mut self, frame: TirFrame) {
        self.vars.truncate(frame.0);
    }

    pub fn create_var(&mut self, node: TirNode<'a>, ty: VRef<Ty>, span: Span) -> VRef<Var> {
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
    pub node: TirNode<'a>,
    pub ty: VRef<Ty>,
    pub span: Span,
}

pub struct TirFrame(usize);

impl TirFrame {
    pub fn contains(&self, var: VRef<Var>) -> bool {
        var.index() >= self.0
    }
}

pub struct Var;

#[derive(Clone, Copy, Debug)]
pub struct AccessTir {
    pub span: Span,
    pub ty: VRef<Ty>,
    pub var: VRef<Var>,
}

#[derive(Clone, Copy, Debug)]
pub struct IntTir {
    pub val: i64,
    pub span: Span,
}

#[derive(Default, Clone, Copy, Debug)]
pub struct BlockTir<'a> {
    pub nodes: &'a [TirNode<'a>],
    pub ty: VRef<Ty>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct ReturnTir<'a> {
    pub val: Option<TirNode<'a>>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct CallTir<'a> {
    pub func: CallableTir<'a>,
    pub params: &'a [VRef<Ty>],
    pub args: &'a [TirNode<'a>],
    pub ty: VRef<Ty>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum CallableTir<'a> {
    Func(VRef<Func>),
    SpecFunc(VRef<SpecFunc>),
    Pointer(TirNode<'a>),
}

#[derive(Clone, Copy, Debug)]
pub struct TypedTirNode<'a> {
    pub node: TirNode<'a>,
    pub ty: VRef<Ty>,
}

#[derive(Clone, Copy, Debug)]
pub struct Variable<'a> {
    pub value: Option<TirNode<'a>>,
    pub ty: VRef<Ty>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct IntLit {
    pub span: Span,
    pub ty: VRef<Ty>,
}

#[derive(Clone, Copy, Debug)]
pub struct ConstructorTir<'a> {
    pub span: Span,
    pub ty: VRef<Ty>,
    pub fields: &'a [TirNode<'a>],
}

#[derive(Clone, Copy, Debug)]
pub enum TirNode<'a> {
    Var(&'a Variable<'a>),
    Int(&'a IntLit),
    Char(Span),
    Block(&'a BlockTir<'a>),
    Return(&'a ReturnTir<'a>),
    Call(&'a CallTir<'a>),
    Access(&'a AccessTir),
    Const(&'a ConstTir<'a>),
    Constructor(&'a ConstructorTir<'a>),
}

#[derive(Clone, Copy, Debug)]
pub struct ConstTir<'a> {
    pub value: TirNode<'a>,
    pub span: Span,
}

pub trait NodeInput<'a> {
    fn convert(self, arena: &'a Arena) -> TirNode<'a>;
}

impl<'a> NodeInput<'a> for TirNode<'a> {
    fn convert(self, _: &'a Arena) -> TirNode<'a> {
        self
    }
}

macro_rules! impl_node_input {
    (
        $(
            $l:lifetime $ty:ty => $variant:ident,
        )*
    ) => {
        $(
            impl<$l> NodeInput<$l> for $ty {
                #[inline]
                fn convert(self, arena: &$l Arena) -> TirNode<$l> {
                    TirNode::$variant(arena.alloc(self))
                }
            }
        )*
    };
}

impl_node_input! {
    'a Variable<'a> => Var,
    'a IntLit => Int,
    'a CallTir<'a> => Call,
    'a AccessTir => Access,
    'a BlockTir<'a> => Block,
    'a ReturnTir<'a> => Return,
    'a ConstTir<'a> => Const,
    'a ConstructorTir<'a> => Constructor,
}
