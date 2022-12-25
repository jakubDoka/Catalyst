#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(default_free_fn)]

mod fmt;
mod length;
mod logic;

pub use {
    fmt::{Fmt, FmtCfg, FmtCtx},
    length::Length,
    middleware::*,
};
