#![feature(result_flattening)]
#![feature(let_chains)]

#[macro_export]
macro_rules! scoped_ident {
    ($module:expr, $item:expr) => {
        ident!($module, "::", $item)
    };
}

mod scope;

pub use scope::{Scope, ScopeError, ScopeItem, ScopePointer};
