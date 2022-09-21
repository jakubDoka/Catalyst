use lexing_t::*;
use storage::*;
use typec_t::*;

pub struct FunctionMir {
    pub blocks_insts: BumpMap<VRef<InstMir>>,
}

pub struct DebugData {
    instr_spans: ShadowMap<InstMir, Span>,
}

pub struct InstMir {
    pub kind: InstKind,
    pub value: Maybe<VRef<ValueMir>>,
}

pub enum InstKind {}

pub struct ValueMir {
    pub ty: VRef<Ty>,
    pub inst: Maybe<VRef<InstMir>>,
}
