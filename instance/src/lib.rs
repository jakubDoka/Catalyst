#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]

#![feature(if_let_guard)]

pub mod state;
pub mod error;
pub mod func;
pub mod repr;

pub use state::{MirBuilder, LayoutBuilder, ReprInstancing};
pub use func::{MirBuilderContext};
pub use repr::{build_builtin_reprs, build_reprs};