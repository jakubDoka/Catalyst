use std::{cell::Cell, default::default, ops::Index};

use lexing_t::*;
use storage::*;

use crate::{BoundFunc, Func, Ty};

pub struct BodyTirBuilder<'a> {
    pub ret: VRef<Ty>,
    pub arena: &'a Arena,
    blocks: BumpVec<IntermediateBlockTir<'a>>,
    vars: BumpVec<ValueTir<'a>>,
}

impl<'a> BodyTirBuilder<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            ret: Ty::UNIT,
            arena,
            blocks: BumpVec::new(),
            vars: BumpVec::new(),
        }
    }

    pub fn ty(&self, ty: VRef<Ty>) -> &'a Cell<VRef<Ty>> {
        self.arena.alloc(Cell::new(ty))
    }

    /// Allocations can be reused after this.
    pub fn build(&mut self) -> Option<BodyTir<'a>> {
        self.vars.clear();

        let blocks = self
            .blocks
            .drain(..)
            .map(|block| block.build(self.arena))
            .collect::<Option<BumpVec<_>>>()?;
        let blocks = self.arena.alloc_slice(blocks.as_slice());

        Some(BodyTir { blocks })
    }

    pub fn create_block(&mut self, input: BlockInputTir<'a>) -> VRef<Block> {
        let id = self.blocks.len();
        self.blocks
            .push(IntermediateBlockTir { input, ..default() });
        unsafe { VRef::new(id) }
    }

    pub fn add_inst(&mut self, block: VRef<Block>, inst: InstTir<'a>) {
        self.blocks[block.index()].stmts.push(inst);
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

impl<'a> Index<VRef<Var>> for BodyTirBuilder<'a> {
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
