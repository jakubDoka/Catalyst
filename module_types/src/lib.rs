#![feature(let_else)]

pub mod module;
pub mod scope;
pub mod error;
pub mod units;

pub use module::{Modules, ModuleItem, ModuleEnt};
pub use scope::{Scope, ScopeCollision, ScopeItem, ScopePointer, ScopeSlot, ItemLexicon};
pub use error::ModuleError;
pub use units::{Unit, UnitEnt, Units};