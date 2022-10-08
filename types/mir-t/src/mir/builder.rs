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

    pub fn inst(&mut self, kind: InstMir, span: Span) -> Option<()> {
        self.current_block?;
        self.ctx.insts.push((kind, span));
        Some(())
    }

    pub fn value(&mut self, ty: Ty, typec: &Typec) -> VRef<ValueMir> {
        if ty == Ty::UNIT {
            return ValueMir::UNIT;
        }
        if ty == Ty::TERMINAL {
            return ValueMir::TERMINAL;
        }

        let ty = self.ctx.project_ty(ty, typec);
        self.ctx.func.values.push(ValueMir {
            ty,
            ..Default::default()
        })
    }

    pub fn close_block(&mut self, span: Span, control_flow: ControlFlowMir) -> bool {
        let Some(current_block) = self.current_block else {
            return true;
        };

        self.increment_block_refcount(control_flow);

        self.ctx.close_block(current_block, control_flow);
        self.ctx.dd.block_closers[current_block] = span;

        false
    }

    fn increment_block_refcount(&mut self, control_flow: ControlFlowMir) {
        match control_flow {
            ControlFlowMir::Return(..) => {}
        }
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
    pub used_types: Map<Ty, VRef<MirTy>>,
    pub just_compiled: Vec<VRef<Func>>,
    pub generic_types: Vec<VRef<MirTy>>,
}

impl MirBuilderCtx {
    pub fn create_block(&mut self) -> VRef<BlockMir> {
        self.func.blocks.push(BlockMir::default())
    }

    pub fn get_var(&self, var: VRef<Var>) -> VRef<ValueMir> {
        self.vars[var.index()]
    }

    pub fn project_ty_slice(&mut self, ty_slice: &[Ty], typec: &Typec) -> VRefSlice<MirTy> {
        self.func.ty_params.bump(ty_slice.iter().map(|&ty| {
            Self::project_ty_low(
                ty,
                &mut self.used_types,
                &mut self.func.dependant_types,
                &mut self.generic_types,
                typec,
            )
        }))
    }

    pub fn project_ty(&mut self, ty: Ty, typec: &Typec) -> VRef<MirTy> {
        Self::project_ty_low(
            ty,
            &mut self.used_types,
            &mut self.func.dependant_types,
            &mut self.generic_types,
            typec,
        )
    }

    pub fn project_ty_low(
        ty: Ty,
        used_types: &mut Map<Ty, VRef<MirTy>>,
        dependant_types: &mut PushMap<MirTy>,
        generic_types: &mut Vec<VRef<MirTy>>,
        typec: &Typec,
    ) -> VRef<MirTy> {
        if let Some(&ty) = used_types.get(&ty) {
            return ty;
        }

        let mir_ty = dependant_types.push(MirTy { ty });
        used_types.insert(ty, mir_ty);

        if typec.contains_params(ty) {
            generic_types.push(mir_ty);
        }

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

            ..self.func.blocks[id] // inherit ref_count
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
        let mut cln = self.func.clone();
        cln.generics = cln.ty_params.bump(self.generic_types.drain(..));
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
