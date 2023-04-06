#![feature(if_let_guard, default_free_fn, iter_intersperse)]
mod context;
mod creation;

pub use {context::TypeCreator, creation::display::TypeDisplay};
