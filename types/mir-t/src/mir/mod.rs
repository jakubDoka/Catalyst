use lexing_t::*;
use storage::*;
use typec_t::*;

pub mod builder;

#[derive(Default, Clone)]
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
        self.values.clear();
        self.value_args.clear();
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
}

#[derive(Clone, Copy)]
pub struct InstMir {
    pub kind: InstKind,
    pub value: Maybe<VRef<ValueMir>>,
}

#[derive(Clone, Copy)]
pub enum InstKind {
    Int(Span),
}

#[derive(Clone, Copy)]
pub struct ValueMir {
    pub ty: VRef<Ty>,
}
