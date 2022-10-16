#![feature(let_else)]
#![feature(default_free_fn)]

mod mir;

pub use mir::{
    builder::{MirBuilder, MirBuilderCtx, VarMir},
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncConstMir, FuncMir, InstMir, Mir,
    MirTy, MirTypeSwapper, ValueMir,
};
