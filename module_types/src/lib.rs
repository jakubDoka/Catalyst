#![feature(let_else)]

pub mod error;
pub mod module;
pub mod scope;
pub mod units;

pub use error::ModuleError;
pub use module::{ModuleEnt, ModuleItem, ModuleMap, Modules};
pub use scope::{Scope, ScopeCollision, ScopeItem, ScopePointer, ScopeSlot};
pub use units::{Unit, UnitEnt, Units};
