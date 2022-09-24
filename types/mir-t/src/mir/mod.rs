use lexing_t::*;
use storage::*;
use typec_t::*;

pub mod builder;

#[derive(Clone)]
pub struct FuncMir {
    pub blocks: PushMap<BlockMir>,
    pub insts: BumpMap<InstMir>,
    pub values: PushMap<ValueMir>,
    pub value_args: BumpMap<VRef<ValueMir>>,
    pub ty_params: BumpMap<VRef<MirTy>>,
    pub dependant_types: PushMap<MirTy>,
}

impl FuncMir {
    pub fn value_ty(&self, value: VRef<ValueMir>) -> VRef<Ty> {
        self.dependant_types[self.values[value].ty].ty
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
        self.insts.clear();
        self.values.truncate(ValueMir::TERMINAL.index() + 1);
        self.value_args.clear();
    }
}

impl Default for FuncMir {
    fn default() -> Self {
        Self {
            blocks: Default::default(),
            insts: Default::default(),
            values: {
                let mut values = PushMap::new();
                values.push(ValueMir { ty: MirTy::UNIT });
                values.push(ValueMir {
                    ty: MirTy::TERMINAL,
                });
                values
            },
            value_args: Default::default(),
            ty_params: Default::default(),
            dependant_types: {
                let mut values = PushMap::new();
                values.push(MirTy { ty: Ty::UNIT });
                values.push(MirTy { ty: Ty::TERMINAL });
                values
            },
        }
    }
}

#[derive(Clone, Copy)]
pub struct MirTy {
    pub ty: VRef<Ty>,
}

impl MirTy {
    gen_increasing_constants! {
        UNIT
        TERMINAL
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
    Return(Maybe<VRef<ValueMir>>),
}

impl Default for ControlFlowMir {
    fn default() -> Self {
        Self::Return(None.into())
    }
}

#[derive(Default)]
pub struct DebugData {
    pub instr_spans: ShadowMap<InstMir, Span>,
    pub block_closers: ShadowMap<BlockMir, Span>,
}

#[derive(Clone, Copy)]
pub enum InstMir {
    Int(i64, VRef<ValueMir>),
    Access(VRef<ValueMir>),
    Call(CallMir, VRef<ValueMir>),
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
    BoundFunc(VRef<BoundFunc>),
    Pointer(VRef<ValueMir>),
}

#[derive(Clone, Copy)]
pub struct ValueMir {
    pub ty: VRef<MirTy>,
}

impl ValueMir {
    gen_increasing_constants!(
        UNIT
        TERMINAL
    );
}
