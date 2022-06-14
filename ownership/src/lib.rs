#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(let_else)]

pub mod drop_solver;
pub mod error;
pub mod ownership_solver;
pub mod state;
pub mod types;

pub use error::OwError;
pub use state::{DropSolver, OwnershipSolver};
pub use types::{
    Access, DropNode, DropNodeEnt, DropNodeList, Ownership, OwnershipContext, OwnershipEnt,
    OwnershipScope,
};
