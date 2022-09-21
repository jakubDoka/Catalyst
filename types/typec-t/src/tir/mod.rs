use lexing_t::*;
use storage::*;

use crate::{BoundFunc, Func, Ty};

pub type TypeCheckedFuncs<'a> = BumpVec<(VRef<Func>, Option<TirNode<'a>>)>;

pub struct TirBuilder<'a> {
    pub arena: &'a Arena,
    pub ret: VRef<Ty>,
    pub ret_span: Maybe<Span>,
    pub vars: BumpVec<TypedTirNode<'a>>,
    pub frames: BumpVec<usize>,
}

impl<'a> TirBuilder<'a> {
    pub fn new(arena: &'a Arena, ret: VRef<Ty>, ret_span: Maybe<Span>) -> Self {
        Self {
            arena,
            ret,
            ret_span,
            vars: BumpVec::new(),
            frames: BumpVec::new(),
        }
    }

    pub fn node<T: NodeInput<'a>>(&mut self, node: T) -> TirNode<'a> {
        node.convert(self.arena)
    }

    pub fn start_frame(&mut self) {
        self.frames.push(self.vars.len());
    }

    pub fn end_frame(&mut self) {
        self.vars.truncate(self.frames.pop().unwrap());
    }

    pub fn create_var(&mut self, node: TirNode<'a>, ty: VRef<Ty>) -> VRef<Var> {
        let index = self.vars.len();
        self.vars.push(TypedTirNode { node, ty });
        unsafe { VRef::new(index) }
    }

    pub fn next_var(&self) -> VRef<Var> {
        unsafe { VRef::new(self.vars.len()) }
    }

    pub fn get_var(&self, var: VRef<Var>) -> TypedTirNode<'a> {
        self.vars[var.index()]
    }
}

pub struct Var;

#[derive(Clone, Copy, Debug)]
pub struct AccessTir<'a> {
    pub node: TirNode<'a>,
    pub span: Span,
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
    pub func_span: Span,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum CallableTir<'a> {
    Func(VRef<Func>),
    BoundFunc(VRef<BoundFunc>),
    Pointer(TirNode<'a>),
}

#[derive(Clone, Copy, Debug)]
pub struct TypedTirNode<'a> {
    pub node: TirNode<'a>,
    pub ty: VRef<Ty>,
}

#[derive(Clone, Copy, Debug)]
pub enum TirNode<'a> {
    Param(VRef<Var>),
    Int(Span),
    Block(&'a BlockTir<'a>),
    Return(&'a ReturnTir<'a>),
    Call(&'a CallTir<'a>),
    Access(&'a AccessTir<'a>),
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
    'a CallTir<'a> => Call,
    'a AccessTir<'a> => Access,
    'a BlockTir<'a> => Block,
    'a ReturnTir<'a> => Return,
}
