#![feature(result_flattening)]
#![feature(let_chains)]

#[macro_export]
macro_rules! scoped_ident {
    ($module:expr, $item:expr) => {
        ident!($module, "::", $item)
    };
}

pub mod scope;
