#![feature(
    try_blocks,
    iter_intersperse,
    slice_group_by,
    if_let_guard,
    default_free_fn,
    never_type,
    let_chains,
    decl_macro,
    btree_drain_filter,
    result_option_inspect,
    iter_collect_into
)]

mod builder;
mod ctx;
mod display;
mod patterns;
mod state_gen;

pub use {
    builder::{compile_constants, compile_functions, MirBuilder, MirCompilationCtx},
    ctx::{ExternalMirCtx, MirBuildMeta, ReusedMirCtx},
    display::{display_function, MirDisplayCtx},
};

/*
    ## Restructuring

    We want a builder that will be constructed with nondefault values, used
    to build a body, and then consumed into body in its final representation.
*/
