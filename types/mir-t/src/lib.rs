#![feature(default_free_fn)]
#![feature(let_chains)]

mod mir;

pub use mir::{
    builder::{
        DependantTypes, EnumMove, MirBuilder, MirBuilderCtx, MirMoveCtx, Move, MoveError,
        MoveFrame, MoveGraph, Owner, VarMir,
    },
    BlockMir, CallMir, CallableMir, ControlFlowMir, DebugData, FuncConstMir, FuncMir, FuncMirInner,
    InstMir, Mir, MirTy, ValueMir,
};
