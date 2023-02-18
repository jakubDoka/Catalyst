use std::{
    default::default,
    ops::{Index, IndexMut},
};

use diags::*;
use span::*;
use mir_t::*;
use resources::*;
use storage::*;
use types::*;
use type_creator::TypeCreator;

use crate::builder::moves::MoveCtx;

pub(crate) struct FuncBorrowcCtx<'m, 'i> {
    pub(crate) generics: &'i [FragSlice<Spec>],
    pub(crate) ret: VRef<ValueMir>,
    pub(crate) unit: VRef<ValueMir>,
    pub(crate) terminal: VRef<ValueMir>,
    pub(crate) module: ModuleMirCheck<'m>,
    pub(crate) module_ref: FragRef<ModuleMir>,
}

impl<'m, 'i> FuncBorrowcCtx<'m, 'i> {
    pub(crate) fn new(
        ret: Ty,
        generics: &'i [FragSlice<Spec>],
        module_ref: FragRef<ModuleMir>,
        module: &'m mut ModuleMir,
        reused: &mut BorrowcCtx,
        types: &Types,
    ) -> Self {
        let mut check = module.check();
        let mut make_value = |ty| {
            check
                .values
                .push(ValueMir::new(reused.intern_ty(ty, &mut check.types, types)))
        };

        Self {
            generics,
            ret: make_value(ret),
            unit: make_value(Ty::UNIT),
            terminal: make_value(Ty::TERMINAL),
            module: check,
            module_ref,
        }
    }

    pub(crate) fn create_value(
        &mut self,
        of: Ty,
        reused: &mut BorrowcCtx,
        types: &Types,
    ) -> VRef<ValueMir> {
        if of == Ty::UNIT {
            self.unit
        } else if of == Ty::TERMINAL {
            self.terminal
        } else {
            self.module.values.push(ValueMir::new(reused.intern_ty(
                of,
                &mut self.module.types,
                types,
            )))
        }
    }

    pub(crate) fn create_params(
        &mut self,
        params: &[Ty],
        reused: &mut BorrowcCtx,
        types: &Types,
    ) -> VSlice<VRef<TyMir>> {
        let iter = params
            .iter()
            .map(|&of| reused.intern_ty(of, &mut self.module.types, types));

        self.module.ty_params.extend(iter)
    }

    pub(crate) fn create_block_with_pass(&mut self, passed: OptVRef<ValueMir>) -> VRef<BlockMir> {
        self.module.blocks.push(BlockMir {
            passed,
            insts: default(),
            control_flow: ControlFlowMir::Return(self.unit),
            ref_count: 0,
            cycles: 0,
        })
    }

    pub(crate) fn create_block(&mut self) -> VRef<BlockMir> {
        self.create_block_with_pass(None)
    }

    pub(crate) fn create_var_from_value(
        &mut self,
        value: VRef<ValueMir>,
        reused: &mut BorrowcCtx,
    ) {
        self.module.values[value].mark_var();
        reused.store_in_var(value);
        reused.vars.push(value);
    }

    pub(crate) fn create_var(
        &mut self,
        ty: Ty,
        reused: &mut BorrowcCtx,
        types: &Types,
    ) -> VRef<ValueMir> {
        let value = self.create_value(ty, reused, types);
        self.create_var_from_value(value, reused);
        value
    }

    pub(crate) fn finish(
        self,
        args: impl IntoIterator<Item = VRef<ValueMir>>,
        entry: VRef<BlockMir>,
        reused: &mut BorrowcCtx,
    ) -> FuncMir {
        let res = FuncMir::new(
            args,
            self.ret,
            reused.generic_types.drain(..),
            entry,
            self.module_ref,
            self.module,
        );

        reused.clear();

        res
    }

    pub(crate) fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.module.types[self.module.values[value].ty()].ty
    }

    pub(crate) fn close_block(
        &mut self,
        current: VRef<BlockMir>,
        _span: Span,
        flow: ControlFlowMir,
        reused: &mut BorrowcCtx,
    ) {
        let insts = self
            .module
            .insts
            .extend(reused.insts.iter().map(|&(i, ..)| i));

        // TODO: handle debug data

        reused.insts.clear();

        self.module.blocks[current].insts = insts;
        self.module.blocks[current].control_flow = flow;
    }

    pub(crate) fn call_inst(
        &mut self,
        callable: CallableMir,
        params: VSlice<VRef<TyMir>>,
        args: impl IntoIterator<Item = VRef<ValueMir>>,
        dest: VRef<ValueMir>,
    ) -> InstMir {
        let args = self.module.value_args.extend(args);
        let callable = self.module.calls.push(CallMir {
            callable,
            params,
            args,
        });
        InstMir::Call(callable, dest)
    }

    pub fn increment_block_refcount(&mut self, control_flow: ControlFlowMir) {
        match control_flow {
            ControlFlowMir::Terminal | ControlFlowMir::Return(..) => {}
            ControlFlowMir::Split {
                then, otherwise, ..
            } => {
                self.module.blocks[then].ref_count += 1;
                self.module.blocks[otherwise].ref_count += 1;
            }
            ControlFlowMir::Goto { dest, .. } => {
                self.module.blocks[dest].ref_count += 1;
            }
        }
    }

    pub fn mark_cycle(&mut self, value: VRef<BlockMir>) {
        self.module.blocks[value].cycles += 1;
    }
}

pub struct ExternalMirCtx<'m, 'i> {
    pub types: &'m mut Types,
    pub interner: &'m mut Interner,
    pub workspace: &'m mut Workspace,

    pub arena: &'i Arena,
    pub resources: &'i Resources,
}

impl<'i, 'm> ExternalMirCtx<'m, 'i> {
    pub(crate) fn creator(&mut self) -> TypeCreator {
        TypeCreator {
            types: self.types,
            interner: self.interner,
        }
    }
}

#[derive(Clone, Copy)]
pub struct BorrowcMeta {
    pub source: VRef<Source>,
    pub module: VRef<Module>,
    pub no_moves: bool,
}

impl BorrowcMeta {
    pub fn source_loc(&self, span: Span) -> SourceLoc {
        SourceLoc {
            origin: self.source,
            span,
        }
    }
}

#[derive(Default)]
pub struct BorrowcCtx {
    used_types: Map<Ty, VRef<TyMir>>,
    generic_types: Vec<VRef<TyMir>>,
    vars: Vec<VRef<ValueMir>>,
    to_drop: Vec<VRef<ValueMir>>,
    insts: Vec<(InstMir, Span)>,
    loops: Vec<LoopMir>,

    pub(crate) moves: MoveCtx,
}

impl BorrowcCtx {
    fn intern_ty(&mut self, ty: Ty, mir_types: &mut PushMapCheck<TyMir>, types: &Types) -> VRef<TyMir> {
        *self.used_types.entry(ty).or_insert_with(|| {
            let res = mir_types.push(TyMir { ty });
            if types.contains_params(ty) {
                self.generic_types.push(res)
            }
            res
        })
    }

    fn clear(&mut self) {
        self.used_types.clear();
        assert!(self.generic_types.is_empty());
        assert!(self.vars.is_empty());
        assert!(self.to_drop.is_empty());
        assert!(self.insts.is_empty(), "{:?}", self.insts);
        assert!(self.loops.is_empty());

        self.moves.clear();
    }

    pub(crate) fn disard_drop_frame(&mut self, frame: DropFrame) {
        self.vars.truncate(frame.base);
        self.to_drop.truncate(frame.to_drop);
    }

    pub(crate) fn create_drop_frame(&mut self) -> DropFrame {
        DropFrame {
            base: self.vars.len(),
            to_drop: self.to_drop.len(),
        }
    }

    pub(crate) fn store_in_var(&mut self, value: VRef<ValueMir>) {
        if self.moves.mark_var(value) {
            self.to_drop.push(value);
        }
    }

    pub(crate) fn inst(&mut self, inst: InstMir, span: Span) {
        self.insts.push((inst, span));
    }

    pub(crate) fn drop_from(&mut self, frame: DropFrame) -> impl Iterator<Item = VRef<ValueMir>> {
        self.vars.truncate(frame.base);
        self.to_drop
            .drain(frame.to_drop..)
            .collect::<BumpVec<_>>()
            .into_iter()
    }

    pub(crate) fn view_drop_from(&self, base: &DropFrame) -> impl Iterator<Item = VRef<ValueMir>> {
        self.to_drop[base.to_drop..].to_bumpvec().into_iter()
    }

    pub(crate) fn var(&self, var: VRef<VarHeaderTir>) -> VRef<ValueMir> {
        self.vars[var.index()]
    }

    pub(crate) fn no_insts(&self) -> bool {
        self.insts.is_empty()
    }

    pub(crate) fn push_loop(&mut self, r#loop: LoopMir) {
        self.loops.push(r#loop);
    }

    pub(crate) fn pop_loop(&mut self) -> LoopMir {
        self.loops.pop().unwrap()
    }
}

impl Index<VRef<LoopHeaderTir>> for BorrowcCtx {
    type Output = LoopMir;

    fn index(&self, index: VRef<LoopHeaderTir>) -> &Self::Output {
        &self.loops[index.index()]
    }
}

impl IndexMut<VRef<LoopHeaderTir>> for BorrowcCtx {
    fn index_mut(&mut self, index: VRef<LoopHeaderTir>) -> &mut Self::Output {
        &mut self.loops[index.index()]
    }
}

#[must_use]
pub(crate) struct DropFrame {
    base: usize,
    to_drop: usize,
}

impl DropFrame {
    pub(crate) const BASE: DropFrame = DropFrame {
        base: 0,
        to_drop: 0,
    };

    pub(crate) unsafe fn clone(&self) -> DropFrame {
        DropFrame {
            base: self.base,
            to_drop: self.to_drop,
        }
    }
}

pub struct LoopMir {
    pub(crate) frame: DropFrame,
    pub(crate) start: VRef<BlockMir>,
    pub(crate) end: OptVRef<BlockMir>,
    pub(crate) dest: VRef<ValueMir>,
    pub(crate) depth: u32,
}
