use crate::{
    tir::{
        block::{self, Block},
        inst::{self, Inst},
        value::{self, Value},
        LinkedList,
    },
    ty::Ty,
};
use cranelift_entity::{packed_option::PackedOption, EntityList, ListPool, PrimaryMap};
use lexer::{Sources, Span};
use parser::ast::{self, Ast};

pub struct Functions {
    pub ents: PrimaryMap<Func, Ent>,
    blocks: PrimaryMap<Block, block::Ent>,
    pub values: PrimaryMap<Value, value::Ent>,
    insts: PrimaryMap<Inst, inst::Ent>,
    value_slices: ListPool<Value>,
}

impl Functions {
    pub fn new() -> Self {
        Functions {
            ents: PrimaryMap::new(),
            blocks: PrimaryMap::new(),
            values: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            value_slices: ListPool::new(),
        }
    }

    pub fn create_block(&mut self, func: Func) -> Block {
        let under = self.ents[func].end.expand();
        let block = self.blocks.push(block::Ent::default());
        let func = &mut self.ents[func];
        self.blocks
            .insert(block, under, &mut func.start, &mut func.end);
        block
    }

    pub fn push_block_param(&mut self, block: Block, param: Value) {
        self.blocks[block].args.push(param, &mut self.value_slices);
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = (Value, &value::Ent)> + '_ {
        self.blocks[block]
            .args
            .as_slice(&self.value_slices)
            .iter()
            .map(|&value| (value, &self.values[value]))
    }

    pub fn add_value(&mut self, value: value::Ent) -> Value {
        self.values.push(value)
    }

    pub fn make_values(&mut self, values: impl Iterator<Item = Value>) -> EntityList<Value> {
        EntityList::from_iter(values, &mut self.value_slices)
    }

    pub fn add_inst(&mut self, func: Func, inst: inst::Ent) -> Inst {
        self.add_inst_to_block(self.ents[func].end.unwrap(), inst)
    }

    pub fn add_inst_to_block(&mut self, block: Block, inst: inst::Ent) -> Inst {
        let last = self.blocks[block].last.expand();
        let inst = self.insts.push(inst);
        let block = &mut self.blocks[block];
        self.insts
            .insert(inst, last, &mut block.first, &mut block.last);
        inst
    }

    pub fn is_block_terminated(&self, block: Block) -> bool {
        self.blocks[block]
            .last
            .expand()
            .map_or(false, |last| self.insts[last].kind.is_terminating())
    }

    pub fn display<'a>(
        &'a self,
        func: Func,
        sources: &'a Sources,
        ast_data: &'a ast::Data,
    ) -> Display<'a> {
        Display {
            func_data: self,
            func,
            _sources: sources,
            _ast_data: ast_data,
        }
    }

    pub fn insts_of(&self, id: Block) -> impl Iterator<Item = (Inst, &inst::Ent)> {
        self.insts.linked_iter(self.blocks[id].first.expand())
    }

    pub fn blocks_of(&self, func: Func) -> impl Iterator<Item = (Block, &block::Ent)> {
        self.blocks.linked_iter(self.ents[func].start.expand())
    }

    pub fn add(&mut self, ent: Ent) -> Func {
        self.ents.push(ent)
    }

    pub fn get(&self, func: Func) -> &Ent {
        &self.ents[func]
    }
}

pub struct Display<'a> {
    func: Func,
    func_data: &'a Functions,
    _sources: &'a Sources,
    _ast_data: &'a ast::Data,
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn {{",)?;
        for (id, _) in self.func_data.blocks_of(self.func) {
            writeln!(f, "  {:?}:", id)?;
            for (id, inst) in self.func_data.insts_of(id) {
                writeln!(f, "    {:?}: {:?}", id, inst)?;
            }
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct Ent {
    pub sig: Signature,
    pub ast: Ast,
    pub return_type: PackedOption<Ty>,
    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    //pub params: EntityList<Ty>,
    pub call_conv: Span,
    pub args: EntityList<Ty>,
    pub ret: PackedOption<Ty>,
}

lexer::gen_entity!(Func);