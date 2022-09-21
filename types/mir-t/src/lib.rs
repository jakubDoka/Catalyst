#![feature(let_else)]

mod mir;

pub use mir::{
    builder::{MirBuilder, MirBuilderCtx},
    BlockMir, ControlFlowMir, DebugData, FuncMir, InstKind, InstMir, ValueMir,
};
