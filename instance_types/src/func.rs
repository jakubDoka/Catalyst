use crate::mir::*;


use storage::*;
use typec_types::*;

#[derive(Debug)]
pub struct Loop {
    pub _enter: Block,
    pub exit: Block,
    pub marker: Tir,
    pub dest: Option<Value>,
}

pub struct FuncCtx {
    pub values: PrimaryMap<Value, ValueEnt>,
    pub value_slices: FramedStackMap<ValueList, Value>,
    pub blocks: PrimaryMap<Block, BlockEnt>,
    pub insts: PrimaryMap<Inst, InstEnt>,
    pub stacks: PrimaryMap<StackSlot, StackEnt>,

    pub current: PackedOption<Block>,
    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
    pub loops: Vec<Loop>,
}

impl FuncCtx {
    pub fn new() -> Self {
        FuncCtx {
            values: PrimaryMap::new(),
            value_slices: FramedStackMap::new(),
            blocks: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            stacks: PrimaryMap::new(),
            current: Default::default(),
            start: Default::default(),
            end: Default::default(),
            loops: Default::default(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        let block = self.blocks.push(Default::default());
        if let Some(end) = self.end.expand() {
            self.blocks[end].next = block.into();
            self.blocks[block].prev = end.into();
        } else {
            self.start = block.into();
        }
        self.end = block.into();
        block
    }

    pub fn add_inst(&mut self, inst: InstEnt) -> Inst {
        assert!(!self.is_terminated());
        let block = &mut self.blocks[self.current.unwrap()];
        let inst = self.insts.push(inst);
        if let Some(end) = block.end.expand() {
            self.insts[end].next = inst.into();
            self.insts[inst].prev = end.into();
        } else {
            block.start = inst.into();
        }
        block.end = inst.into();
        inst
    }

    pub fn blocks(&self) -> impl Iterator<Item = (Block, &BlockEnt)> + '_ {
        self.blocks.linked_iter(self.start.expand())
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = (Value, &ValueEnt)> + '_ {
        self.value_slices
            .get(self.blocks[block].params)
            .iter()
            .map(|&value| (value, &self.values[value]))
    }

    pub fn clear(&mut self) {
        self.values.clear();
        self.value_slices.clear();
        self.blocks.clear();
        self.insts.clear();
        self.stacks.clear();
        self.start.take();
        self.end.take();
    }

    pub fn is_terminated(&self) -> bool {
        self.current.expand().map_or(false, |current| {
            self.blocks[current]
                .end
                .expand()
                .map_or(false, |end| self.insts[end].kind.is_terminating())
        })
    }

    pub fn select_block(&mut self, block: Block) {
        self.current = block.into();
    }
}
