#![feature(type_name_of_val)]
#![feature(if_let_guard)]

/// Shorthand for common result type
pub type Result<T = ()> = std::result::Result<T, ()>;