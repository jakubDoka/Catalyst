#![feature(type_name_of_val)]
#![feature(if_let_guard)]

pub extern crate lsp_types;

/// Shorthand for common result type
pub type Result<T = ()> = std::result::Result<T, ()>;