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
}

impl FuncMir {
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
                values.push(ValueMir { ty: Ty::UNIT });
                values.push(ValueMir { ty: Ty::TERMINAL });
                values
            },
            value_args: Default::default(),
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct BlockMir {
    pub args: VRefSlice<ValueMir>,
    pub insts: VSlice<InstMir>,
    pub control_flow: ControlFlowMir,
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
pub struct InstMir {
    pub kind: InstKind,
    pub value: Maybe<VRef<ValueMir>>,
}

#[derive(Clone, Copy)]
pub enum InstKind {
    Int(Span),
    Access(VRef<ValueMir>),
}

#[derive(Clone, Copy)]
pub struct ValueMir {
    pub ty: VRef<Ty>,
}

impl ValueMir {
    gen_increasing_constants!(
        UNIT
        TERMINAL
    );
}
