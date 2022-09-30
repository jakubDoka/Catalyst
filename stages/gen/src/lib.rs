#![feature(let_else)]

mod context;
mod generate;
mod jit_context;
mod object_context;

mod state_gen;

pub use {
    crate::object_context::{ObjectContext, ObjectCreationError},
    context::{
        CodeSaveError, CompileRequest, CompileRequests, CompiledFunc, Gen, GenBlock, GenBuilder,
        GenFuncConstant, GenItemName, GenLayouts, GenReloc, GenResources, Isa, IsaCreationError,
        Layout, Offset,
    },
    jit_context::{JitContext, JitRelocError},
    state_gen::Generator,
};

pub const ENTRY_POINT_NAME: &str = "__catalyst_main__";
