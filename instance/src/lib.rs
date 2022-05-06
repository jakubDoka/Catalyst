#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(bool_to_option)]

pub mod error;
pub mod func;
pub mod repr;

pub use func::MirBuilder;
pub use repr::ReprBuilder;