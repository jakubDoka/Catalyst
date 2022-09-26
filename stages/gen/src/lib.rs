#![feature(let_else)]

mod context;
mod generate;
mod jit;
mod object;

mod state_gen;

pub use {
    context::{
        perform_jit_relocations, CompileRequest, CompileRequests, CompiledFunc, Gen, GenBlock,
        GenItemName, GenLayouts, GenReloc, GenResources, Layout, Offset,
    },
    jit::JitContext,
    state_gen::Generator,
};
