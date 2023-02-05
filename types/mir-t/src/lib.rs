#![feature(default_free_fn)]
#![feature(let_chains)]

mod mir;

pub use mir::{
    BlockMir, BodyOwner, CallMir, CallableMir, ControlFlowMir, DebugData, DropMir, FuncMir,
    FuncTypes, FuncValues, InstMir, Mir, MirBase, MirTy, ModuleMir, ValueMir,
};
