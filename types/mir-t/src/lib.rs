#![feature(default_free_fn)]
#![feature(let_chains)]

mod mir;

pub use mir::{
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncConstMir, FuncMir, FuncMirInner,
    FuncTypes, FuncValues, InstMir, Mir, MirTy, ValueMir,
};
