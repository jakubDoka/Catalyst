#![feature(let_else)]
#![feature(associated_type_defaults)]
#![feature(const_type_id)]

#[macro_export]
macro_rules! gen_scope_lookup {
    (
        $(
            $name:ident<$item_name:literal $(, $output:ty)?> {
                $($id:ty => $ty_name:literal,)*
            }
        )*
    ) => {
        $(
            pub struct $name;

            impl $crate::ScopeLookup for $name {
                $(type Output = $output;)?
                const ITEM_NAME: &'static str = $item_name;
                const TYPE_MISMATCH_MAPPING: &'static [(TypeId, &'static str)] = &[
                    $(
                        (TypeId::of::<$id>(), $ty_name),
                    )*
                ];
            }
        )*
    };
}

mod state_gen;
mod ty_parser;

pub use state_gen::{ItemCollector, TyBuilder, TyParser};
pub use ty_parser::{ScopeLookup, TyLookup};
