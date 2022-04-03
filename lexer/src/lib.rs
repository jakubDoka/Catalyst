#![feature(auto_traits)]
#![feature(negative_impls)]

pub extern crate cranelift_entity;

mod logic;
pub mod map;
mod source_info;
pub mod token;

pub mod prelude {
    pub use crate::logic::Lexer;
    pub use crate::source_info::*;
    pub use crate::token::Token;
}

#[macro_export]
macro_rules! gen_entity {
    ($($name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct $name(pub u32);

            $crate::cranelift_entity::entity_impl!($name);

            impl Default for $name {
                fn default() -> Self {
                    $crate::cranelift_entity::packed_option::ReservedValue::reserved_value()
                }
            }

            impl $crate::map::IDFilter for $name {}
        )*
    };
}
