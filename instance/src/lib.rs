#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(if_let_guard)]

pub mod error;
pub mod func;
pub mod repr;
pub mod state;

pub use func::MirBuilderContext;
pub use repr::{build_builtin_reprs, build_reprs};
pub use state::{LayoutBuilder, MirBuilder, ReprInstancing};
