use std::mem;

use lexing_t::Span;
use storage::*;
use typec_t::*;

use crate::*;

pub struct MirBuilder<'a> {
    pub current_block: Option<VRef<BlockMir>>,
    pub ctx: &'a mut MirBuilderCtx,
}

impl<'a> MirBuilder<'a> {
    pub fn new(entry_block: VRef<BlockMir>, ctx: &'a mut MirBuilderCtx) -> Self {
        Self {
            current_block: Some(entry_block),
            ctx,
        }
    }

    pub fn inst(
        &mut self,
        kind: InstKind,
        ty: Option<VRef<Ty>>,
        span: Span,
    ) -> Option<Option<VRef<ValueMir>>> {
        self.current_block?;

        let value = ty.map(|ty| self.ctx.func.values.push(ValueMir { ty }));
        self.ctx.insts.push((
            InstMir {
                kind,
                value: value.into(),
            },
            span,
        ));
        Some(value)
    }

    pub fn dump(&mut self, control_flow: ControlFlowMir) -> bool {
        let Some(current_block) = self.current_block else {
            return true;
        };

        self.ctx.dump(current_block, control_flow);

        false
    }

    pub fn select_block(&mut self, block: VRef<BlockMir>) -> bool {
        mem::replace(&mut self.current_block, Some(block)).is_some()
    }
}

#[derive(Default)]
pub struct MirBuilderCtx {
    pub func: FuncMir,
    pub dd: DebugData,
    pub vars: Vec<VRef<ValueMir>>,
    pub args: Vec<VRef<ValueMir>>,
    pub insts: Vec<(InstMir, Span)>,
}

impl MirBuilderCtx {
    pub fn create_block(&mut self) -> VRef<BlockMir> {
        self.func.blocks.push(BlockMir::default())
    }

    pub fn dump(&mut self, id: VRef<BlockMir>, control_flow: ControlFlowMir) {
        let block = BlockMir {
            args: self.func.value_args.bump(self.args.drain(..)),
            insts: self
                .func
                .insts
                .bump(self.insts.iter().map(|&(inst, ..)| inst)),
            control_flow,
        };

        self.func
            .insts
            .indexed(block.insts)
            .zip(self.insts.drain(..).map(|(.., span)| span))
            .for_each(|((key, ..), span)| self.dd.instr_spans[key] = span);

        self.func.blocks[id] = block;
    }

    pub fn clear(&mut self) -> FuncMir {
        self.vars.clear();
        let cln = self.func.clone();
        self.func.clear();
        cln
    }
}
