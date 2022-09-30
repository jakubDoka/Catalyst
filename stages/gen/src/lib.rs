#![feature(let_else)]

mod context;
mod generate;
mod jit_context;
mod object_context;

mod state_gen;

pub use {
    crate::object_context::{ObjectContext, ObjectCreationError},
    context::{
        perform_jit_relocations, CompileRequest, CompileRequests, CompiledFunc, Gen, GenBlock,
        GenFuncConstant, GenItemName, GenLayouts, GenReloc, GenResources, Layout, Offset,
    },
    jit_context::JitContext,
    state_gen::Generator,
};

pub const ENTRY_POINT_NAME: &str = "__catalyst_main__";
