#![feature(let_else)]
#![feature(associated_type_defaults)]
#![feature(const_type_id)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(let_chains)]
#![feature(never_type)]
#![feature(iter_intersperse)]

#[macro_export]
macro_rules! gen_scope_lookup {
    (
        $(
            $name:ident<$item_name:literal $(, $output:ty $(, $field:ident)?)?> {
                $($id:ty => $ty_name:literal,)*
            }
        )*
    ) => {
        $(
            pub struct $name;

            impl $crate::ScopeLookup for $name {
                $(type Output = $output;)?
                const ITEM_NAME: &'static str = $item_name;
                const TYPE_MISMATCH_MAPPING: &'static [(std::any::TypeId, &'static str)] = &[
                    $(
                        (TypeId::of::<$id>(), $ty_name),
                    )*
                ];

                $($(
                    fn index(typec: &Typec, id: VRef<str>) -> Option<VRef<Self::Output>> {
                        typec.$field.index(id)
                    }
                )?)?
            }
        )*
    };
}

#[macro_export]
macro_rules! insert_scope_item {
    () => {};
}

mod func_builder;
mod item_collector;
mod state_gen;
mod tir_display;
mod ty_builder;
mod ty_parser;

pub use util::{build_scope, duplicate_definition, insert_scope_item, TyCheckerCtx};

pub use item_collector::CollectGroup;
pub use state_gen::TyChecker;
pub use ty_parser::{ScopeLookup, TyLookup};

mod util {
    use diags::*;
    use lexing_t::*;
    use packaging_t::*;
    use parsing::*;
    use scope::*;
    use storage::*;
    use typec_t::*;

    #[derive(Default)]
    pub struct TyCheckerCtx {
        pub structs: TypecOutput<Ty>,
        pub funcs: TypecOutput<Func>,
        pub tir_arena: Arena,
        pub extern_funcs: Vec<VRef<Func>>,
    }

    impl TyChecker<'_> {
        pub fn execute<'a>(
            &mut self,
            items: GroupedItemsAst,
            ctx: &'a mut TyCheckerCtx,
            type_checked_funcs: &mut &'a [(VRef<Func>, TirNode<'a>)],
        ) -> &mut Self {
            ctx.tir_arena.clear();
            self.collect(items.structs, Self::collect_struct, &mut ctx.structs)
                .collect(items.funcs, Self::collect_func, &mut ctx.funcs)
                .build_structs(items.structs, &mut ctx.structs)
                .build_funcs(
                    items.funcs,
                    &ctx.tir_arena,
                    &mut ctx.funcs,
                    type_checked_funcs,
                    &mut ctx.extern_funcs,
                )
        }
    }

    pub fn build_scope(module: VRef<str>, scope: &mut Scope, packages: &Packages, typec: &Typec) {
        scope.clear();

        for &ty in Ty::ALL {
            let id = typec.types.id(ty);
            scope.insert_builtin(id, ty);
        }

        let mod_ent = packages.modules.get(&module).unwrap();
        for dep in &packages.conns[mod_ent.deps] {
            let mod_ent = packages.modules.get(&dep.ptr).unwrap();
            let ModKind::Module { ref items, .. } = mod_ent.kind else {
                unreachable!();
            };
            let r#mod = packages.ident_as_mod(dep.ptr).unwrap();
            let item = ModItem::new(dep.name, r#mod, dep.name_span, dep.name_span, Vis::Priv);
            scope.insert_current(item.to_scope_item(dep.ptr)).unwrap();
            for &item in items {
                scope.insert(module, item.to_scope_item(dep.ptr)).unwrap();
            }
        }
    }

    use crate::TyChecker;

    impl TyChecker<'_> {
        pub fn insert_scope_item(&mut self, item: packaging_t::ModItem) {
            crate::insert_scope_item(
                item,
                self.scope,
                self.current_file,
                self.packages,
                self.workspace,
            );
        }
    }

    pub fn insert_scope_item(
        item: ModItem,
        scope: &mut Scope,
        current_file: VRef<str>,
        packages: &mut Packages,
        workspace: &mut Workspace,
    ) {
        if let Err(spans) = scope.insert(current_file, item.to_scope_item(current_file)) {
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
        file: VRef<str>,
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
