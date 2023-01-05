#![feature(result_option_inspect)]
#![feature(default_free_fn)]
#![feature(let_chains)]
mod logic;

pub use {
    logic::{CcCtx, CcRuntime},
    middleware::*,
};
