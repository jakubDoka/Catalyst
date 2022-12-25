#![allow(incomplete_features)]
#![feature(specialization)]

mod length;
mod logic;

pub use {
    length::Length,
    logic::{Fmt, FmtCfg, FmtCtx},
    middleware::*,
};
