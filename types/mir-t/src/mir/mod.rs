use std::{
    ops::{Deref, DerefMut},
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

#[derive(Clone, Default)]
pub struct FuncMirInner {
    pub ret: VRef<ValueMir>,
    pub generics: VRefSlice<MirTy>,
    pub blocks: PushMap<BlockMir>,
    pub insts: PushMap<InstMir>,
    pub values: FuncValues,
    pub value_args: PushMap<VRef<ValueMir>>,
    pub ty_params: PushMap<VRef<MirTy>>,
    pub calls: PushMap<CallMir>,
    pub types: FuncTypes,
    pub drops: PushMap<DropMir>,
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
        self.values.clear();
        self.value_args.clear();
        self.ty_params.clear();
        self.types.clear();
        self.calls.clear();
        self.drops.clear();
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

#[derive(Clone, Copy, Debug)]
pub struct DropMir {
    pub value: VRef<ValueMir>,
}

#[derive(Clone, Copy)]
pub struct MirTy {
    pub ty: Ty,
}

impl MirTy {
    gen_v_ref_constants!(
        UNIT
        TERMINAL
    );
}

#[derive(Clone, Copy, Default)]
pub struct BlockMir {
    pub args: VRefSlice<ValueMir>,
    pub insts: VSlice<InstMir>,
    pub control_flow: ControlFlowMir,
    pub ref_count: u16,
    pub cycles: u16,
}

#[derive(Clone, Copy)]
pub enum ControlFlowMir {
    Split(VRef<ValueMir>, VRef<BlockMir>, VRef<BlockMir>),
    Goto(VRef<BlockMir>, VRef<ValueMir>),
    Return(VRef<ValueMir>),
    Terminal,
}

impl Default for ControlFlowMir {
    fn default() -> Self {
        Self::Return(ValueMir::UNIT)
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
    Call(VRef<CallMir>, VRef<ValueMir>),
    Ctor(VRefSlice<ValueMir>, VRef<ValueMir>, bool),
    Deref(VRef<ValueMir>, VRef<ValueMir>),
    Ref(VRef<ValueMir>, VRef<ValueMir>),
    Field(VRef<ValueMir>, u32, VRef<ValueMir>),
    Bool(bool, VRef<ValueMir>),
    Drop(VRef<DropMir>),
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

impl VRefDefault for ValueMir {
    fn default_state() -> VRef<Self> {
        Self::UNIT
    }
}

impl ValueMir {
    gen_v_ref_constants!(
        UNIT
        TERMINAL
    );
}

#[derive(Clone)]
pub struct FuncTypes(PushMap<MirTy>);

impl FuncTypes {
    pub fn new() -> Self {
        let mut pm = PushMap::new();
        pm.push(MirTy { ty: Ty::UNIT });
        pm.push(MirTy { ty: Ty::TERMINAL });
        Self(pm)
    }

    pub fn clear(&mut self) {
        self.0.truncate(ValueMir::ALL.len());
    }
}

impl Default for FuncTypes {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for FuncTypes {
    type Target = PushMap<MirTy>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FuncTypes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone)]
pub struct FuncValues(PushMap<ValueMir>);

impl FuncValues {
    pub fn new() -> Self {
        let mut pm = PushMap::new();
        pm.push(ValueMir { ty: MirTy::UNIT });
        pm.push(ValueMir {
            ty: MirTy::TERMINAL,
        });
        Self(pm)
    }

    pub fn clear(&mut self) {
        self.0.truncate(ValueMir::TERMINAL.index() + 1);
    }
}

impl Default for FuncValues {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for FuncValues {
    type Target = PushMap<ValueMir>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FuncValues {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
