use std::{
    cell::Cell,
    default::default,
    ops::{Deref, DerefMut, Index},
};

use lexing_t::*;
use storage::*;

use crate::{BoundFunc, Func, Ty};

pub struct TirBuilder<'a, 'b> {
    pub ret: VRef<Ty>,
    pub current_block: VRef<Block>,
    pub arena: &'a Arena,
    builder: &'b mut TirBuilderCtx<'a>,
}

impl<'a, 'b> TirBuilder<'a, 'b> {
    pub fn new(
        arena: &'a Arena,
        builder: &'b mut TirBuilderCtx<'a>,
        ret: VRef<Ty>,
        current_block: VRef<Block>,
    ) -> Self {
        Self {
            ret,
            current_block,
            arena,
            builder,
        }
    }

    pub fn ty(&self, ty: VRef<Ty>) -> &'a Cell<VRef<Ty>> {
        self.arena.alloc(Cell::new(ty))
    }

    pub fn inst(&mut self, ty: VRef<Ty>, inst: impl InstInput<'a>) -> ValueTir<'a> {
        let inst = inst.convert(self.arena);
        self.builder.blocks[self.current_block.index()]
            .stmts
            .push(inst);
        let ty = self.ty(ty);
        ValueTir {
            inst: Some(inst),
            ty,
        }
    }

    pub fn build(&mut self) -> Option<BodyTir<'a>> {
        self.builder.build(self.arena)
    }

    pub fn close_block(&mut self, control_flow: ControlFlowTir<'a>) {
        self.builder.blocks[self.current_block.index()].control_flow = Some(control_flow);
    }

    pub fn terminal(&mut self) -> ValueTir<'a> {
        ValueTir {
            inst: None,
            ty: self.ty(Ty::TERMINAL),
        }
    }
}

pub trait InstInput<'a> {
    fn convert(self, arena: &'a Arena) -> InstTir<'a>;
}

macro_rules! impl_inst_input {
    (
        $(
            $l:lifetime $ty:ty |$self:ident, $arena:ident| $expr:expr;
        )*
    ) => {
        $(

            impl<$l> InstInput<$l> for $ty {
                fn convert($self, $arena: &$l Arena) -> InstTir<$l> {
                    $expr
                }
            }
        )*
    };
}

impl<'a> Deref for TirBuilder<'a, '_> {
    type Target = TirBuilderCtx<'a>;

    fn deref(&self) -> &Self::Target {
        self.builder
    }
}

impl<'a> DerefMut for TirBuilder<'a, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.builder
    }
}

#[derive(Default)]
pub struct TirBuilderCtx<'a> {
    blocks: BumpVec<IntermediateBlockTir<'a>>,
    vars: BumpVec<ValueTir<'a>>,
}

impl<'a> TirBuilderCtx<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocations can be reused after this.
    pub fn build(&mut self, arena: &'a Arena) -> Option<BodyTir<'a>> {
        self.vars.clear();

        let blocks = self
            .blocks
            .drain(..)
            .map(|block| block.build(arena))
            .collect::<Option<BumpVec<_>>>()?;
        let blocks = arena.alloc_slice(blocks.as_slice());

        Some(BodyTir { blocks })
    }

    pub fn create_block(&mut self, input: BlockInputTir<'a>) -> VRef<Block> {
        let id = self.blocks.len();
        self.blocks
            .push(IntermediateBlockTir { input, ..default() });
        unsafe { VRef::new(id) }
    }

    pub fn close_block(&mut self, block: VRef<Block>, control_flow: ControlFlowTir<'a>) {
        self.blocks[block.index()].control_flow = Some(control_flow);
    }

    pub fn create_var(&mut self, value: ValueTir<'a>) -> VRef<Var> {
        let id = self.vars.len();
        self.vars.push(value);
        unsafe { VRef::new(id) }
    }
}

impl<'a> Index<VRef<Var>> for TirBuilderCtx<'a> {
    type Output = ValueTir<'a>;

    fn index(&self, index: VRef<Var>) -> &Self::Output {
        &self.vars[index.index()]
    }
}

#[derive(Default)]
struct IntermediateBlockTir<'a> {
    input: BlockInputTir<'a>,
    stmts: BumpVec<InstTir<'a>>,
    control_flow: Option<ControlFlowTir<'a>>,
}

impl<'a> IntermediateBlockTir<'a> {
    fn build(self, arena: &'a Arena) -> Option<BlockTir<'a>> {
        Some(BlockTir {
            input: self.input,
            stmts: arena.alloc_slice(self.stmts.as_slice()),
            control_flow: self.control_flow?,
        })
    }
}

pub struct Var;

#[derive(Clone, Copy, Debug)]
pub struct BodyTir<'a> {
    pub blocks: &'a [BlockTir<'a>],
}

impl<'a> Index<VRef<Block>> for BodyTir<'a> {
    type Output = BlockTir<'a>;

    fn index(&self, index: VRef<Block>) -> &Self::Output {
        &self.blocks[index.index()]
    }
}

pub struct Block;

#[derive(Clone, Copy, Debug, Default)]
pub struct BlockTir<'a> {
    pub input: BlockInputTir<'a>,
    pub stmts: &'a [InstTir<'a>],
    pub control_flow: ControlFlowTir<'a>,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum BlockInputTir<'a> {
    #[default]
    None,
    Single(ValueTir<'a>),
    FuncArgs(&'a [ValueTir<'a>]),
}

#[derive(Clone, Copy, Debug)]
pub enum InstTir<'a> {
    Int(Span),
    Call(&'a CallTir<'a>),
}

impl_inst_input! {
    'a CallTir<'a> |self, arena| InstTir::Call(arena.alloc(self));
    'a InstTir<'a> |self, _arena| self;
}

#[derive(Clone, Copy, Debug)]
pub struct CallTir<'a> {
    pub func: FuncTir<'a>,
    pub params: &'a [VRef<Ty>],
    pub args: &'a [ValueTir<'a>],
}

#[derive(Clone, Copy, Debug)]
pub enum FuncTir<'a> {
    Concrete(VRef<Func>),
    Virtual(VRef<BoundFunc>),
    Indirect(ValueTir<'a>),
}

#[derive(Clone, Copy, Debug)]
pub enum ControlFlowTir<'a> {
    Return(Option<ValueTir<'a>>),
    Branch(BranchTir<'a>),
    Jump(JumpTir<'a>),
}

impl<'a> ControlFlowTir<'a> {
    pub fn ret(value: ValueTir<'a>) -> Self {
        Self::Return((value.ty.get() != Ty::UNIT).then_some(value))
    }
}

impl Default for ControlFlowTir<'_> {
    fn default() -> Self {
        Self::Return(None)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BranchTir<'a> {
    pub cond: ValueTir<'a>,
    pub then: VRef<Block>,
    pub r#else: VRef<Block>,
}

#[derive(Clone, Copy, Debug)]
pub struct JumpTir<'a> {
    pub target: VRef<Block>,
    pub value: Option<ValueTir<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ValueTir<'a> {
    pub inst: Option<InstTir<'a>>,
    pub ty: &'a Cell<VRef<Ty>>,
}
