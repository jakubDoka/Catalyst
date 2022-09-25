#![feature(let_else)]

mod context;
mod generate;
mod native;

mod state_gen;

pub use {
    context::{CompileRequest, CompiledFunc, GenBlock, GeneratorCtx, Layout, Offset},
    state_gen::Generator,
};
