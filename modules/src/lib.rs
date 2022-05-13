#![feature(let_else)]
#![feature(result_flattening)]
#![feature(bool_to_option)]

pub extern crate cranelift_entity;

pub mod error;
pub mod manifest;
pub mod module;
pub mod unit;

pub use module::ModuleBuilder;
pub use unit::{LoaderContext, UnitBuilder};

//
