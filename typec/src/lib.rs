#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]

pub mod error;
pub mod func;
pub mod logic;
pub mod tir;
pub mod ty;

pub use func::*;
pub use logic::*;
pub use ty::*;

#[macro_export]
macro_rules! gen_context {
    ($name:ident<$($lifetime:lifetime),*> { $($field_name:ident: $field_type:ty),* $(,)?}) => {
        pub struct $name<$($lifetime),*> {
            $(pub $field_name: $field_type),*
        }

        impl<$($lifetime),*> From<($($field_type),*)> for $name<$($lifetime),*> {
            fn from(($($field_name),*): ($($field_type),*)) -> Self {
                Self {
                    $($field_name),*
                }
            }
        }
    };
}
