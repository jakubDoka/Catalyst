#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(default_free_fn)]
#![feature(let_chains)]
#![feature(decl_macro)]

mod fmt;
mod length;
mod logic;

pub use {
    fmt::{Fmt, FmtCfg, FmtCtx, SpaceReplacer},
    length::Length,
    logic::{FmtRuntime, FmtRuntimeCtx},
    middleware::*,
};
