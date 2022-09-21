use lexing_t::*;
use storage::*;
use typec_t::*;

pub struct FunctionMir {
    pub blocks_insts: BumpMap<VRef<InstMir>>,
    pub blocks: PushMap<BlockTir>,
    pub insts: PushMap<IntTir>,
    pub values: PushMap<ValueMir>,
    pub value_args: BumpMap<VRef<ValueMir>>,
}

pub struct BlockTir {
    pub args: VSlice<ValueMir>,
    pub insts: VRefSlice<InstMir>,
    pub control_flow: ControlFlow,
}

pub enum ControlFlow {
    Return(Maybe<VRef<ValueMir>>),
}

pub struct DebugData {
    pub instr_spans: ShadowMap<InstMir, Span>,
}

pub struct InstMir {
    pub kind: InstKind,
    pub value: Maybe<VRef<ValueMir>>,
}

pub enum InstKind {
    Int(Span),
}

pub struct ValueMir {
    pub ty: VRef<Ty>,
    pub inst: Maybe<VRef<InstMir>>,
}
