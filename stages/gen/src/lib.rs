#![feature(let_else)]

mod context;
mod generate;
mod jit;
mod object;

mod state_gen;

pub use {
    crate::object::{ObjectContext, ObjectCreationError},
    context::{
        perform_jit_relocations, CompileRequest, CompileRequests, CompiledFunc, Gen, GenBlock,
        GenFuncConstant, GenItemName, GenLayouts, GenReloc, GenResources, Layout, Offset,
    },
    jit::JitContext,
    state_gen::Generator,
};

pub const ENTRY_POINT_NAME: &str = "__catalyst_main__";
