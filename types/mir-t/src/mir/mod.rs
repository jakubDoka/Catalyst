use std::{
    ops::Deref,
    sync::Arc,
};

use lexing_t::*;

use storage::*;
use typec_t::*;

#[derive(Default, Clone)]
pub struct Mir {
    pub bodies: CMap<FragRef<Func>, FuncMir>,
}

impl Mir {
    pub fn clear(&mut self) {
        self.bodies.clear();
    }
}

impl Clear for Mir {
    fn clear(&mut self) {
        self.bodies.clear();
    }
}

#[derive(Clone, Default)]
pub struct FuncMir {
    pub inner: Arc<FuncMirInner>,
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

pub struct MirFuncCheckPoint {
    blocks: usize,
    insts: usize,
    values: usize,
    value_args: usize,
    ty_params: usize,
    calls: usize,
    types: usize,
}

#[derive(Clone, Default)]
pub struct FuncMirInner {
    pub ret: OptVRef<ValueMir>,
    pub generics: VRefSlice<MirTy>,
    pub blocks: PushMap<BlockMir>,
    pub insts: PushMap<InstMir>,
    pub values: PushMap<ValueMir>,
    pub value_args: PushMap<VRef<ValueMir>>,
    pub ty_params: PushMap<VRef<MirTy>>,
    pub calls: PushMap<CallMir>,
    pub types: PushMap<MirTy>,
    value_flags: BitSet,
}

impl FuncMirInner {
    const FLAG_WIDTH: usize = 2;
    const IS_REFERENCED: usize = 0;
    const IS_MUTABLE: usize = 1;

    pub fn check_point(&self) -> MirFuncCheckPoint {
        MirFuncCheckPoint {
            blocks: self.blocks.len(),
            insts: self.insts.len(),
            values: self.values.len(),
            value_args: self.value_args.len(),
            ty_params: self.ty_params.len(),
            calls: self.calls.len(),
            types: self.types.len(),
        }
    }

    pub fn rollback(&mut self, check_point: MirFuncCheckPoint) {
        self.blocks.truncate(check_point.blocks);
        self.insts.truncate(check_point.insts);
        self.values.truncate(check_point.values);
        self.value_args.truncate(check_point.value_args);
        self.ty_params.truncate(check_point.ty_params);
        self.calls.truncate(check_point.calls);
        self.types.truncate(check_point.types);
        self.value_flags.truncate(check_point.values * Self::FLAG_WIDTH);
    }

    pub fn clear(&mut self) {
        self.ret = None;
        self.generics = VRefSlice::default();
        self.blocks.clear();
        self.insts.clear();
        self.values.clear();
        self.value_args.clear();
        self.ty_params.clear();
        self.types.clear();
        self.calls.clear();
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

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.types[self.values[value].ty].ty
    }
}

#[derive(Clone, Copy)]
pub struct MirTy {
    pub ty: Ty,
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
    Call(VRef<CallMir>, OptVRef<ValueMir>),
    Const(VRef<FuncConstMir>, VRef<ValueMir>),
    Ctor(VRefSlice<ValueMir>, VRef<ValueMir>, bool),
    Deref(VRef<ValueMir>, VRef<ValueMir>),
    Ref(VRef<ValueMir>, VRef<ValueMir>),
    Field(VRef<ValueMir>, u32, VRef<ValueMir>),
    Bool(bool, VRef<ValueMir>),
    MayDrop(VRef<ValueMir>),
    Drop(VRef<ValueMir>, FragRef<Impl>),
}

#[derive(Clone, Copy)]
pub struct CallMir {
    pub callable: CallableMir,
    pub params: VRefSlice<MirTy>,
    pub args: VRefSlice<ValueMir>,
}

#[derive(Clone, Copy, Debug)]
pub enum CallableMir {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>),
    Pointer(VRef<ValueMir>),
}

#[derive(Clone, Copy)]
pub struct ValueMir {
    pub ty: VRef<MirTy>,
}