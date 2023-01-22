use lexing_t::Span;

use storage::*;
use typec_t::*;

use mir_t::*;

#[derive(Clone, Copy)]
pub struct VarMir {
    pub value: VRef<ValueMir>,
}

#[derive(Default)]
pub struct MirCtx {
    pub depth: u32,
    pub no_moves: bool,
    pub ret: VRef<ValueMir>,
    pub module: ModuleMirInner,
    pub dd: DebugData,
    pub vars: Vec<VarMir>,
    pub generics: Vec<FragSlice<Spec>>,
    pub insts: Vec<(InstMir, Span)>,
    pub used_types: Map<Ty, VRef<MirTy>>,
    pub just_compiled: Vec<FragRef<Func>>,
    pub generic_types: Vec<VRef<MirTy>>,
    pub value_depths: ShadowMap<ValueMir, u32>,
    pub to_drop: Vec<VRef<ValueMir>>,
    pub loops: Vec<LoopMir>,
    pub types: FuncTypes,
}

impl MirCtx {
    pub fn value(&mut self, ty: Ty, typec: &Typec) -> VRef<ValueMir> {
        let ty = self.project_ty(ty, typec);
        let val = self.module.values.push(ValueMir { ty });
        self.value_depths[val] = self.depth;
        val
    }

    pub fn create_block(&mut self) -> VRef<BlockMir> {
        self.module.blocks.push(BlockMir::default())
    }

    pub fn get_var(&self, var: VRef<VarHeaderTir>) -> VarMir {
        self.vars[var.index()]
    }

    pub fn project_ty_slice(&mut self, ty_slice: &[Ty], typec: &Typec) -> VRefSlice<MirTy> {
        self.module.ty_params.extend(ty_slice.iter().map(|&ty| {
            Self::project_ty_low(
                ty,
                &mut self.used_types,
                &mut self.types,
                &mut self.generic_types,
                typec,
            )
        }))
    }

    pub fn project_ty(&mut self, ty: Ty, typec: &Typec) -> VRef<MirTy> {
        Self::project_ty_low(
            ty,
            &mut self.used_types,
            &mut self.types,
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
            insts: self
                .module
                .insts
                .extend(self.insts.iter().map(|&(inst, ..)| inst)),
            control_flow,

            ..self.module.blocks[id] // inherit ref_count
        };

        self.module
            .insts
            .indexed(block.insts)
            .zip(self.insts.drain(..).map(|(.., span)| span))
            .for_each(|((key, ..), span)| self.dd.instr_spans[key] = span);

        self.module.blocks[id] = block;
    }

    pub fn clear(
        &mut self,
        entry: VRef<BlockMir>,
        module: FragRef<ModuleMir>,
        args: VRefSlice<ValueMir>,
        prev_drops: usize,
        prev_calls: usize,
    ) -> FuncMir {
        assert!(self.vars.is_empty());
        assert!(self.to_drop.is_empty());
        self.dd.clear();
        self.used_types.clear();
        self.generics.clear();

        self.used_types
            .extend([(Ty::UNIT, MirTy::UNIT), (Ty::TERMINAL, MirTy::TERMINAL)]);

        let types = self.module.save_types(&self.types);
        self.types.clear();
        FuncMir {
            ret: self.ret,
            args,
            generics: self.module.ty_params.extend(self.generic_types.drain(..)),
            types,
            entry,
            module,
            calls: VSlice::new(prev_calls..self.module.calls.len()),
            drops: VSlice::new(prev_drops..self.module.drops.len()),
        }
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.types[self.module.values[value].ty].ty
    }
}

#[must_use]
pub struct MirVarFrame {
    pub base: usize,
    pub to_drop: usize,
}

pub struct LoopMir {
    pub frame: MirVarFrame,
    pub start: VRef<BlockMir>,
    pub end: OptVRef<BlockMir>,
    pub dest: VRef<ValueMir>,
    pub depth: u32,
}
