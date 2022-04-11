use cranelift_codegen::{ir, isa::CallConv, packed_option::PackedOption};
use cranelift_entity::{EntityList, ListPool, PrimaryMap};
use lexer::Span;
use typec::tir::LinkedList;

use crate::mir::{
    block::{self, Block},
    inst::{self, Inst},
    value::{self, Value},
};

#[derive(Debug)]
pub struct Function {
    pub name: Span,
    pub signature: ir::Signature,

    pub values: PrimaryMap<Value, value::Ent>,
    pub value_slices: ListPool<Value>,
    pub blocks: PrimaryMap<Block, block::Ent>,
    pub insts: PrimaryMap<Inst, inst::Ent>,

    pub current: PackedOption<Block>,
    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            name: Span::default(),
            signature: ir::Signature::new(CallConv::Fast),
            values: PrimaryMap::new(),
            value_slices: ListPool::new(),
            blocks: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            current: Default::default(),
            start: Default::default(),
            end: Default::default(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        let block = block::Ent {
            ..Default::default()
        };
        let block = self.blocks.push(block);
        self.blocks
            .insert(block, self.end.expand(), &mut self.start, &mut self.end);

        block
    }

    pub fn push_block_param(&mut self, block: Block, param: Value) {
        self.blocks[block]
            .params
            .push(param, &mut self.value_slices);
    }

    pub fn add_inst(&mut self, inst: inst::Ent) -> Inst {
        let inst = self.insts.push(inst);
        let block = self.current.unwrap();
        let block = &mut self.blocks[block];
        self.insts
            .insert(inst, block.last.expand(), &mut block.first, &mut block.last);
        inst
    }

    pub fn blocks(&self) -> impl Iterator<Item = (Block, &block::Ent)> + '_ {
        self.blocks.linked_iter(self.start.expand())
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = (Value, &value::Ent)> + '_ {
        self.blocks[block]
            .params
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

    pub fn make_values(&mut self, arg_iter: impl Iterator<Item = Value>) -> EntityList<Value> {
        EntityList::from_iter(arg_iter, &mut self.value_slices)
    }

    pub fn values(&self, list: EntityList<Value>) -> &[Value] {
        list.as_slice(&self.value_slices)
    }

    pub fn select_block(&mut self, block: Block) {
        self.current = block.into();
    }
}
