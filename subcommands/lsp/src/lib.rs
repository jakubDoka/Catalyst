#![feature(default_free_fn)]
#![feature(result_option_inspect)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(let_chains)]

mod logic;

pub use {
    logic::{LspArgs, LspRuntime},
    middleware::*,
};
