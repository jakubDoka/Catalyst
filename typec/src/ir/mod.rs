use cranelift_entity::{PrimaryMap, packed_option::PackedOption, ListPool, EntityList};
use lexer::prelude::Sources;
use parser::ast;

use crate::{ty::Ty, func::Signature, logic::TypeManager};

use self::{block::Block, inst::Inst, value::Value};

pub mod block;
pub mod inst;
pub mod value;

pub struct Function {
    pub sig: Signature,
    blocks: PrimaryMap<Block, block::Ent>,
    values: PrimaryMap<Value, value::Ent>,
    insts: PrimaryMap<Inst, inst::Ent>,
    value_slices: ListPool<Value>,
    pub return_type: PackedOption<Ty>,
    start: PackedOption<Block>,
    end: PackedOption<Block>,
}

impl Function {
    pub fn new() -> Self {
        Function {
            sig: Signature::default(),
            blocks: PrimaryMap::new(),
            values: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            value_slices: ListPool::new(),
            return_type: None.into(),
            start: None.into(),
            end: None.into(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        let block = block::Ent {
            prev: self.end,
            ..Default::default()
        };
        let block = self.blocks.push(block);
        if let Some(last) = self.end.expand() {
            self.blocks[last].next = Some(block).into();
        } else {
            self.start = Some(block).into();
        }
        self.end = Some(block).into();
        block
    }

    pub fn push_block_param(&mut self, block: Block, param: Value) {
        self.blocks[block].args.push(param, &mut self.value_slices);
    }

    pub fn block_params(&self, block: Block) -> &[Value] {
        self.blocks[block].args.as_slice(&self.value_slices)
    }

    pub fn add_value(&mut self, value: value::Ent) -> Value {
        self.values.push(value)
    }

    pub fn make_values(&mut self, values: impl Iterator<Item = Value>) -> EntityList<Value> {
        EntityList::from_iter(values, &mut self.value_slices)
    }

    pub fn add_inst(&mut self, inst: inst::Ent) -> Inst {
        self.add_inst_to_block(self.end.unwrap(), inst)
    }

    pub fn add_inst_to_block(&mut self, block: Block, mut inst: inst::Ent) -> Inst {
        let block_ent = &mut self.blocks[block];
        inst.prev = block_ent.last;
        let inst = self.insts.push(inst);
        
        if let Some(last) = block_ent.last.expand() {
            assert!(!self.insts[last].kind.is_terminating());
            self.insts[last].next = Some(inst).into();
        } else {
            block_ent.first = Some(inst).into();
        }
        block_ent.last = Some(inst).into();

        inst
    }

    pub fn is_block_terminated(&self, block: Block) -> bool {
        self.blocks[block].last.expand()
            .map_or(false, |last| self.insts[last].kind.is_terminating())
    }

    pub fn finalize(&mut self) {
        for (_, block) in self.blocks.iter() {
            if let Some(last) = block.last.expand() {
                assert!(self.insts[last].kind.is_terminating());
            }
        }
    }

    pub fn display<'a>(&'a self, sources: &'a TypeManager, ast_data: &'a ast::Data) -> Display<'a> {
        Display { func: self, sources, ast_data }
    }

    fn insts_of(&self, id: Block) -> Vec<(Inst, inst::Ent)> {
        let mut vec = vec![];
        let mut current = self.blocks[id].first.expand();
        while let Some(inst) = current {
            vec.push((inst, self.insts[inst]));
            current = self.insts[inst].next.expand();
        }
        vec
    }
}

pub struct Display<'a> {
    func: &'a Function,
    sources: &'a TypeManager,
    ast_data: &'a ast::Data,
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn {{",)?;
        for (id, _) in self.func.blocks.iter() {
            writeln!(f, "  {:?}:", id)?;
            for (id, inst) in self.func.insts_of(id) {
                writeln!(f, "    {:?}: {:?}", id, inst)?;
            }
        }
        Ok(())
    }
}