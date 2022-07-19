#![feature(result_flattening)]
#![feature(let_chains)]

#[macro_export]
macro_rules! scoped_ident {
    ($module:expr, $item:expr) => {
        ident!($module, "::", $item)
    };
}

#[macro_export]
macro_rules! intern_scoped_ident {
    ($self:expr, $str:expr) => {
        $self.interner.intern(
            scoped_ident!(($self.current_file.index() as u32), $str)
        )
    };
}

mod scope;

pub use scope::{Scope, ScopeError, ScopeItem, ScopePtr};
