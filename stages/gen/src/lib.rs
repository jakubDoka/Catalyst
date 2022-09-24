#![feature(let_else)]

mod generate;
mod module_packing;

mod state_gen;

pub use {
    generate::{CompileRequest, CompiledFunc, GenBlock, GeneratorCtx, Layout, Offset},
    module_packing::{DATA_NAMESPACE, FUNC_NAMESPACE},
    state_gen::Generator,
};
