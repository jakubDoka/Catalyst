use cranelift_codegen::{ir, packed_option::PackedOption, isa::CallConv};
use cranelift_entity::{EntityList, ListPool, PrimaryMap};
use typec::tir::LinkedList;

use crate::mir::{
    block::{self, Block},
    inst::{self, Inst},
    value::{self, Value},
};

#[derive(Debug)]
pub struct Function {
    pub signature: ir::Signature,

    pub values: PrimaryMap<Value, value::Ent>,
    pub value_slices: ListPool<Value>,
    pub blocks: PrimaryMap<Block, block::Ent>,
    pub insts: PrimaryMap<Inst, inst::Ent>,

    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            signature: ir::Signature::new(CallConv::Fast),
            values: PrimaryMap::new(),
            value_slices: ListPool::new(),
            blocks: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            start: Default::default(),
            end: Default::default(),
        }
    }

    pub fn create_block(&mut self, params: impl Iterator<Item = value::Ent>) -> Block {
        let params = EntityList::from_iter(
            params.map(|value| self.values.push(value)),
            &mut self.value_slices,
        );

        let block = block::Ent {
            params,
            ..Default::default()
        };
        let block = self.blocks.push(block);
        self.blocks
            .insert(block, self.start.expand(), &mut self.start, &mut self.end);

        block
    }

    pub fn add_inst(&mut self, inst: inst::Ent) -> Inst {
        let inst = self.insts.push(inst);
        let block = self.end.unwrap();
        let block = &mut self.blocks[block];
        self.insts
            .insert(inst, block.last.expand(), &mut block.first, &mut block.last);
        inst
    }

    pub fn blocks(&self) -> impl Iterator<Item = (Block, &block::Ent)> + '_ {
        self.blocks.linked_iter(self.start.expand())
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = (Value, &value::Ent)> + '_ {
        self.blocks[block].params
            .as_slice(&self.value_slices)
            .iter()
            .map(|&value| (value, &self.values[value]))
    }

    pub fn clear(&mut self) {
        self.signature.clear(cranelift_codegen::isa::CallConv::Fast);
        self.values.clear();
        self.value_slices.clear();
        self.blocks.clear();
        self.insts.clear();
        self.start.take();
        self.end.take();
    }
}
