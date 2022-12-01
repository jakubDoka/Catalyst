#![feature(default_free_fn)]
#![feature(let_chains)]

mod mir;

pub use mir::{
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, DropMir, FuncMir, FuncMirInner,
    FuncTypes, FuncValues, InstMir, Mir, MirTy, ValueMir,
};
