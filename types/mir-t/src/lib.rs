#![feature(let_else)]

mod mir;

pub use mir::{
    builder::{MirBuilder, MirBuilderCtx, VarMir},
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncConstMir, FuncMir, InstMir, Mir,
    MirTy, MirTypeSwapper, ValueMir,
};
