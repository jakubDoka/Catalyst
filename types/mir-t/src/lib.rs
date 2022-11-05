#![feature(default_free_fn)]

mod mir;

pub use mir::{
    builder::{
        DependantTypes, MirBuilder, MirBuilderCtx, MirFrame, MirMoveCtx, Move, MoveError,
        MoveFrame, MoveGraph, Owner, VarMir,
    },
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncConstMir, FuncMir, FuncMirInner,
    InstMir, Mir, MirTy, ValueMir,
};
