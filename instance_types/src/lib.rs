pub mod error;
pub mod func;
pub mod mir;
pub mod repr;

pub use error::InstError;
pub use func::{FuncCtx, Loop};
pub use mir::{
    Block, BlockEnt, Inst, InstEnt, InstKind, LinkedList, LinkedNode, MirDisplay, MirFlags,
    MutLinkNode, StackEnt, StackSlot, Value, ValueEnt, ValueList,
};
pub use repr::{ReplaceCache, ReprEnt, ReprField, ReprFields, ReprFlags, Reprs};
