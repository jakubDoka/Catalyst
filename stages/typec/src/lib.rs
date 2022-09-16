#![feature(let_else)]
#![feature(associated_type_defaults)]
#![feature(const_type_id)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]

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

#[macro_export]
macro_rules! insert_scope_item {
    () => {
        pub fn insert_scope_item(&mut self, item: packaging_t::ModItem) {
            $crate::insert_scope_item(
                item,
                &mut self.scope,
                self.current_file,
                &mut self.interner,
                &mut self.packages,
                &mut self.workspace,
            );
        }
    };
}

mod item_collector;
mod state_gen;
mod ty_builder;
mod ty_parser;

pub use util::{duplicate_definition, insert_scope_item};

pub use state_gen::{ItemCollector, TyBuilder, TyParser};
pub use ty_parser::{ScopeLookup, TyLookup};

mod util {
    use diags::*;
    use lexing_t::*;
    use packaging_t::*;
    use scope::*;
    use storage::*;

    pub fn insert_scope_item(
        item: ModItem,
        scope: &mut Scope,
        current_file: VRef<str>,
        interner: &mut Interner,
        packages: &mut Packages,
        workspace: &mut Workspace,
    ) {
        if let Err(spans) = scope.insert(current_file, item.to_scope_item(current_file), interner) {
            workspace.push(duplicate_definition(
                item.whole_span,
                item.span,
                spans,
                current_file,
            ));
            return;
        }

        packages
            .modules
            .get_mut(&current_file)
            .unwrap()
            .add_item(item);
    }

    gen_error_fn!(duplicate_definition(
        duplicate: Span,
        duplicate_name: Span,
        existing: Option<(Span, Span)>,
        file: Ident,
    ) {
        err: "duplicate definition";
        (duplicate, file) {
            info[duplicate_name]: "this name";
        }
        (existing?.0, file) {
            info[existing?.1]: "matches this already existing item";
        }
    });
}
