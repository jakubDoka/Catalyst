#![feature(try_blocks)]
#![feature(iter_intersperse)]
#![feature(slice_group_by)]
#![feature(if_let_guard)]
#![feature(default_free_fn)]
#![feature(never_type)]
#![feature(let_chains)]
#![feature(decl_macro)]

mod builder;
mod ctx;
mod display;
mod moves;
mod patterns;
mod state_gen;

pub use {
    ctx::{MirCtx, MirVarFrame, VarMir},
    moves::MirMoveCtx,
    patterns::{Branch, Node, PatNode, PatNodeChildren, PatTree, Range, UpperBound},
    state_gen::MirChecker,
};
