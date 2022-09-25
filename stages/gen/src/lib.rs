#![feature(let_else)]

mod context;
mod generate;
mod native;

mod state_gen;

pub use {
    context::{
        CompileRequest, CompileRequests, CompiledFunc, Gen, GenBlock, GenItemName, GenLayouts,
        GenReloc, GenResources, Layout, Offset,
    },
    state_gen::Generator,
};
