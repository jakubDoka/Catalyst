#![feature(default_free_fn)]
#![feature(let_chains)]

mod mir;

pub use mir::{
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncConstMir, FuncMir, FuncMirInner,
    InstMir, Mir, MirTy, ValueMir,
};
