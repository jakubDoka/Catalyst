#![feature(let_else)]

mod mir;

pub use mir::{
    builder::{MirBuilder, MirBuilderCtx},
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncMir, InstMir, Mir, MirTy,
    ValueMir,
};
