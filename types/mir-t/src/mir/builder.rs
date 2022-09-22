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

    pub fn inst(&mut self, kind: InstKind, ty: VRef<Ty>, span: Span) -> Option<VRef<ValueMir>> {
        self.current_block?;
        let ty = self.ctx.project_ty(ty);
        let value = self.ctx.func.values.push(ValueMir { ty });
        self.ctx.insts.push((
            InstMir {
                kind,
                value: value.into(),
            },
            span,
        ));
        Some(value)
    }

    pub fn valueless_inst(&mut self, kind: InstKind, span: Span) -> Option<()> {
        self.current_block?;
        self.ctx.insts.push((
            InstMir {
                kind,
                value: None.into(),
            },
            span,
        ));
        Some(())
    }

    pub fn close_block(&mut self, span: Span, control_flow: ControlFlowMir) -> bool {
        let Some(current_block) = self.current_block else {
            return true;
        };

        self.ctx.close_block(current_block, control_flow);
        self.ctx.dd.block_closers[current_block] = span;

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
    pub used_types: ShadowMap<Ty, Maybe<VRef<MirTy>>>,
}

impl MirBuilderCtx {
    pub fn create_block(&mut self) -> VRef<BlockMir> {
        self.func.blocks.push(BlockMir::default())
    }

    pub fn get_var(&self, var: VRef<Var>) -> VRef<ValueMir> {
        self.vars[var.index()]
    }

    pub fn project_ty(&mut self, ty: VRef<Ty>) -> VRef<MirTy> {
        if let Some(ty) = self.used_types[ty].expand() {
            return ty;
        }

        let mir_ty = self.func.dependant_types.push(MirTy { ty });
        self.used_types[ty] = mir_ty.into();

        mir_ty
    }

    pub fn close_block(&mut self, id: VRef<BlockMir>, control_flow: ControlFlowMir) {
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

    pub fn start_frame(&self) -> MirFrame {
        MirFrame(self.vars.len())
    }

    pub fn end_frame(&mut self, frame: MirFrame) {
        self.vars.truncate(frame.0);
    }
}

pub struct MirFrame(usize);
