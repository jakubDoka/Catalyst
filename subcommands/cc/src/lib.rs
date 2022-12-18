#![feature(result_option_inspect)]
#![feature(default_free_fn)]

mod logic;

pub use {logic::CcRuntime, middleware::*};
