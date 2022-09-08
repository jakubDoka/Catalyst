#![feature(let_else)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]

mod packages;
mod state_gen;

pub use state_gen::PackageLoader;
