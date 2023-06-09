#![feature(
    const_trait_impl,
    let_chains,
    slice_group_by,
    default_free_fn,
    iter_collect_into,
    decl_macro,
    trivial_bounds,
    closure_lifetime_binder,
    array_zip
)]

mod logic;

pub use {
    ast::*,
    borrowc::*,
    cli,
    diags::*,
    gen::*,
    lexing::*,
    logic::{
        task::Task,
        worker::{AstHandler, BaseSourceCtx, MacroSourceCtx},
        CommandInfo, DiagnosticView, Middleware, MiddlewareArgs, MiddlewareArgsError,
        MiddlewareOutput, PackageTask, QuickTimer,
    },
    mir::*,
    packaging::*,
    parsing::*,
    resources::*,
    snippet_display::*,
    span::*,
    storage::*,
    types::*,
    types::*,
};
