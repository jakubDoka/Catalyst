//! Crate implements project loader and simple package manager via git.

#![feature(
    default_free_fn,
    anonymous_lifetime_in_impl_trait,
    never_type,
    let_chains,
    iter_intersperse,
    iter_collect_into
)]

mod packages;

pub use packages::{PackageLoader, ResourceLoaderCtx};
