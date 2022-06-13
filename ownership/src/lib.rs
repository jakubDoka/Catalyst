#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(let_else)]

pub mod drop_solver;
pub mod error;
pub mod ownership_solver;
pub mod state;
pub mod types;

pub use state::{DropSolver, OwnershipSolver};
pub use types::{
    Ownership, OwnershipContext, OwnershipEnt, OwnershipScope, 
    DropNode, DropNodeEnt, DropNodeList, Access,
};
pub use error::OwError;