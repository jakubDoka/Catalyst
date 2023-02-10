#![feature(default_free_fn)]
#![feature(let_chains)]

mod mir;

pub use mir::{
    swap_mir_types, BlockMir, BodyOwner, CallMir, CallableMir, ControlFlowMir, DebugData, DropMir,
    FuncMir, FuncMirEntities, FuncMirEntitiesView, FuncMirView, InstMir, Mir, MirBase, MirTy,
    ModuleMir, ModuleMirCheck, ValueMir,
};
