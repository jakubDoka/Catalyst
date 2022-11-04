use std::{default::default, ops::Deref, sync::Arc};

use lexing_t::*;

use storage::*;
use typec_t::*;

use crate::*;

pub mod builder;

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

#[derive(Clone)]
pub struct FuncMirInner {
    pub ret: VRef<ValueMir>,
    pub generics: VRefSlice<MirTy>,
    pub blocks: PushMap<BlockMir>,
    pub insts: PushMap<InstMir>,
    pub values: PushMap<ValueMir>,
    pub value_args: PushMap<VRef<ValueMir>>,
    pub ty_params: PushMap<VRef<MirTy>>,
    pub constants: PushMap<FuncConstMir>,
    pub calls: PushMap<CallMir>,
    pub types: DependantTypes,
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
            calls: default(),
            types: default(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct MirTy {
    pub ty: Ty,
}

impl MirTy {
    gen_v_ref_constants! {
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
    Call(VRef<CallMir>, OptVRef<ValueMir>),
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

#[derive(Clone, Copy, Debug)]
pub enum CallableMir {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>),
    Pointer(VRef<ValueMir>),
}

#[derive(Clone, Copy, Default)]
pub struct ValueMir {
    pub ty: VRef<MirTy>,
}

impl ValueMir {
    gen_v_ref_constants!(
        UNIT
        TERMINAL
    );
}

impl VRefDefault for ValueMir {
    fn default_state() -> VRef<Self> {
        Self::UNIT
    }
}
