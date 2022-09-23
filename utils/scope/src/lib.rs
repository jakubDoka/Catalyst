#![feature(result_flattening)]
#![feature(default_free_fn)]
#![feature(let_else)]

#[macro_export]
macro_rules! scoped_ident {
    ($module:expr, $item:expr) => {
        ident!($module, "\\", $item)
    };
}

#[macro_export]
macro_rules! intern_scoped_ident {
    ($self:expr, $str:expr) => {
        $self
            .interner
            .intern(scoped_ident!(($self.current_file.index() as u32), $str))
    };
}

mod scope;

pub use scope::{Item, Scope, ScopeError, ScopeItem, ScopePtr, Vis};
