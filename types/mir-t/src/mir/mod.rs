use std::{default::default, ops::Deref, sync::Arc};

use lexing_t::*;
use storage::*;
use typec_t::*;

use crate::*;

pub mod builder;

#[derive(Default)]
pub struct Mir {
    pub bodies: SparseMap<Func, FuncMir>,
}

#[derive(Clone, Default)]
pub struct FuncMir {
    pub inner: Arc<FuncMirInner>,
    pub dependant_types: DependantTypes,
}

impl FuncMir {
    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.dependant_types[self.values[value].ty].ty
    }
}

impl Deref for FuncMir {
    type Target = FuncMirInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Clone, Copy)]
pub struct FuncConstMir {
    pub ty: VRef<MirTy>,
    pub block: VRef<BlockMir>,
}

#[derive(Default)]
pub struct MirTypeSwapper {
    swapped: Vec<Ty>,
}

impl MirTypeSwapper {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn swap(
        &mut self,
        func: &mut FuncMir,
        params: &[Ty],
        typec: &mut Typec,
        interner: &mut Interner,
    ) {
        if params.is_empty() {
            return;
        }

        for &mir_ty in &func.inner.ty_params[func.generics] {
            let ty = func.dependant_types[mir_ty].ty;
            let new_ty = typec.instantiate(ty, params, interner);
            func.dependant_types[mir_ty].ty = new_ty;
            self.swapped.push(ty);
        }
    }

    pub fn swap_back(&mut self, func: &mut FuncMir) {
        for (ty, &mir_ty) in self
            .swapped
            .drain(..)
            .zip(&func.inner.ty_params[func.generics])
        {
            func.dependant_types[mir_ty].ty = ty;
        }
    }
}

#[derive(Clone)]
pub struct FuncMirInner {
    pub ret: VRef<ValueMir>,
    pub generics: VRefSlice<MirTy>,
    pub blocks: PushMap<BlockMir>,
    pub insts: BumpMap<InstMir>,
    pub values: PushMap<ValueMir>,
    pub value_args: BumpMap<VRef<ValueMir>>,
    pub ty_params: BumpMap<VRef<MirTy>>,
    pub constants: PushMap<FuncConstMir>,
    value_flags: BitSet,
}

impl FuncMirInner {
    const FLAG_WIDTH: usize = 2;
    const IS_REFERENCED: usize = 0;
    const IS_MUTABLE: usize = 1;

    pub fn clear(&mut self) {
        self.ret = VRef::default();
        self.generics = VRefSlice::default();
        self.blocks.clear();
        self.insts.clear();
        self.values.truncate(ValueMir::TERMINAL.index() + 1);
        self.value_args.clear();
        self.ty_params.clear();
        self.constants.clear();
        self.value_flags.clear();
    }

    pub fn is_referenced(&self, value: VRef<ValueMir>) -> bool {
        self.value_flags
            .contains(value.index() * Self::FLAG_WIDTH + Self::IS_REFERENCED)
    }

    pub fn set_referenced(&mut self, value: VRef<ValueMir>) {
        self.value_flags
            .insert(value.index() * Self::FLAG_WIDTH + Self::IS_REFERENCED);
    }

    pub fn is_mutable(&self, value: VRef<ValueMir>) -> bool {
        self.value_flags
            .contains(value.index() * Self::FLAG_WIDTH + Self::IS_MUTABLE)
    }

    pub fn set_mutable(&mut self, value: VRef<ValueMir>) {
        self.value_flags
            .insert(value.index() * Self::FLAG_WIDTH + Self::IS_MUTABLE);
    }
}

impl Default for FuncMirInner {
    fn default() -> Self {
        Self {
            ret: default(),
            generics: default(),
            blocks: default(),
            insts: default(),
            values: {
                let mut values = PushMap::new();
                values.push(ValueMir { ty: MirTy::UNIT });
                values.push(ValueMir {
                    ty: MirTy::TERMINAL,
                });
                values
            },
            value_args: default(),
            ty_params: default(),
            constants: default(),
            value_flags: default(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct MirTy {
    pub ty: Ty,
}

impl MirTy {
    gen_increasing_constants! {
        UNIT
        TERMINAL
    }
}

impl VRefDefault for MirTy {
    fn default_state() -> VRef<Self> {
        Self::UNIT
    }
}

#[derive(Clone, Copy, Default)]
pub struct BlockMir {
    pub args: VRefSlice<ValueMir>,
    pub insts: VSlice<InstMir>,
    pub control_flow: ControlFlowMir,
    pub ref_count: u32,
}

#[derive(Clone, Copy)]
pub enum ControlFlowMir {
    Split(VRef<ValueMir>, VRef<BlockMir>, VRef<BlockMir>),
    Goto(VRef<BlockMir>, OptVRef<ValueMir>),
    Return(OptVRef<ValueMir>),
    Terminal,
}

impl Default for ControlFlowMir {
    fn default() -> Self {
        Self::Return(None)
    }
}

#[derive(Default)]
pub struct DebugData {
    pub instr_spans: ShadowMap<InstMir, Span>,
    pub block_closers: ShadowMap<BlockMir, Span>,
}

impl DebugData {
    pub fn clear(&mut self) {
        self.instr_spans.clear();
        self.block_closers.clear();
    }
}

#[derive(Clone, Copy)]
pub enum InstMir {
    Var(VRef<ValueMir>, VRef<ValueMir>),
    Int(i64, VRef<ValueMir>),
    Access(VRef<ValueMir>, OptVRef<ValueMir>),
    Call(CallMir, OptVRef<ValueMir>),
    Const(VRef<FuncConstMir>, VRef<ValueMir>),
    Ctor(VRefSlice<ValueMir>, VRef<ValueMir>, bool),
    Deref(VRef<ValueMir>, VRef<ValueMir>),
    Ref(VRef<ValueMir>, VRef<ValueMir>),
    Field(VRef<ValueMir>, u32, VRef<ValueMir>),
    Bool(bool, VRef<ValueMir>),
}

#[derive(Clone, Copy)]
pub struct CallMir {
    pub callable: CallableMir,
    pub params: VRefSlice<MirTy>,
    pub args: VRefSlice<ValueMir>,
}

#[derive(Clone, Copy)]
pub enum CallableMir {
    Func(VRef<Func>),
    SpecFunc(VRef<SpecFunc>),
    Pointer(VRef<ValueMir>),
}

#[derive(Clone, Copy, Default)]
pub struct ValueMir {
    pub ty: VRef<MirTy>,
}

impl ValueMir {
    gen_increasing_constants!(
        UNIT
        TERMINAL
    );
}

impl VRefDefault for ValueMir {
    fn default_state() -> VRef<Self> {
        Self::UNIT
    }
}
