#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]

pub mod error;
pub mod func;
pub mod mir;
pub mod size;
pub mod types;

pub use error::*;
pub use func::*;
pub use mir::*;
pub use size::*;
pub use types::*;

pub type Result<T = ()> = std::result::Result<T, Error>;