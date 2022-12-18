#![feature(default_free_fn)]
#![feature(result_option_inspect)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(let_chains)]
#![feature(drain_filter)]

mod logic;

pub use {
    logic::{LspArgs, LspRuntime},
    middleware::*,
};
