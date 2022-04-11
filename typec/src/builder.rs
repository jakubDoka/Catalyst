use lexer::Span;

use crate::{
    func,
    tir::{block, inst, value, Block, Inst, LinkedList, Value},
    Func, Funcs, Ty,
};

pub struct Builder<'a> {
    pub funcs: &'a mut Funcs,
    pub func: Func,
    pub block: Option<Block>,
}

impl<'a> Builder<'a> {
    pub fn create_block(&mut self) -> Block {
        let func = &mut self.funcs.ents[self.func];
        let block = self.funcs.blocks.push(block::Ent::default());
        self.funcs
            .blocks
            .insert(block, func.end.expand(), &mut func.start, &mut func.end);
        block
    }

    pub fn select_block(&mut self, block: Block) {
        self.block = block.into();
    }

    pub fn push_block_param(&mut self, block: Block, param: Value) {
        self.funcs.blocks[block]
            .args
            .push(param, &mut self.funcs.value_slices);
    }

    pub fn add_inst(
        &mut self,
        kind: inst::Kind,
        value: impl Into<Option<Value>>,
        span: Span,
    ) -> Inst {
        let block = &mut self.funcs.blocks[self.block.unwrap()];
        let ent = inst::Ent::new(kind, value.into(), span);
        let inst = self.funcs.insts.push(ent);
        self.funcs
            .insts
            .insert(inst, block.last.expand(), &mut block.first, &mut block.last);
        inst
    }

    pub fn add_value(&mut self, ty: Ty, span: Span) -> Value {
        let ent = value::Ent::new(ty, span);
        self.funcs.values.push(ent)
    }

    pub fn is_closed(&self) -> bool {
        self.block.map_or(false, |block| {
            self.funcs.blocks[block]
                .last
                .expand()
                .map_or(false, |inst| self.funcs.insts[inst].kind.is_terminating())
        })
    }

    pub fn func_ent(&mut self) -> &mut func::Ent {
        &mut self.funcs.ents[self.func]
    }
}
