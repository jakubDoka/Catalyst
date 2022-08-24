#![feature(let_else)]
#![feature(default_free_fn)]
#![feature(decl_macro)]
#![allow(incomplete_features)]
#![feature(inline_const_pat)]
#![feature(const_type_id)]

#[macro_export]
macro_rules! scope_error_handler {
    () => {
        scope_error_handler!(any);
        scope_error_handler!(concrete);
    };

    (any) => {
        pub fn get_from_scope(
            &mut self,
            id: Ident,
            span: lexing_t::Span,
            message: &str,
            report: ReportSig,
        ) -> errors::Result<ScopePtr> {
            self.scope
                .get(id)
                .map_err(|err| {
                    crate::handle_scope_error(
                        err,
                        span,
                        self.current_file,
                        id,
                        report,
                        message,
                        &self.packages,
                        &mut self.interner,
                        &self.scope,
                        &mut self.workspace,
                    )
                })
                .and_then(|item| {
                    check_vis(item, span, self.current_file, self.packages, self.workspace)
                })
                .map(|item| item.ptr)
        }
    };

    (concrete) => {
        pub fn get_from_scope_concrete<T: VPtr + 'static>(
            &mut self,
            id: Ident,
            span: lexing_t::Span,
            message: &str,
            report: ReportSig,
        ) -> errors::Result<T> {
            self.scope
                .get_concrete::<T>(id)
                .map_err(|err| {
                    crate::handle_scope_error(
                        err,
                        span,
                        self.current_file,
                        id,
                        report,
                        message,
                        &self.packages,
                        &mut self.interner,
                        &self.scope,
                        &mut self.workspace,
                    )
                })
                .and_then(|(id, item)| {
                    check_vis(item, span, self.current_file, self.packages, self.workspace)
                        .map(|_| id)
                })
        }
    };
}

mod fn_parser;
mod item_collector;
mod state_gen;
mod ty_builder;
mod ty_parser;

pub use state_gen::{FuncParser, ItemCollector, TyBuilder, TyParser};
pub use utils::{check_vis, handle_scope_error, ReportSig, Reports};

mod utils {
    use std::any::TypeId;

    use diags::*;
    use inner_lexing::*;
    use packaging_t::*;
    use scope::*;
    use storage::*;
    use type_checking_t::*;

    pub fn check_vis(
        item: scope::Item,
        span: Span,
        module: Ident,
        packages: &Packages,
        workspace: &mut Workspace,
    ) -> errors::Result<scope::Item> {
        if let Some(item_module) = item.module.expand() {
            if let Err(err) = packages.check_vis(item_module, module, item.vis) {
                workspace.push(diag! {
                    (span, module) => "cannot access this item because of visibility restrictions",
                    (span, item_module) => "item is defined here",
                    (none) => "if you have control over pinpointed code, {} will fix the issue" {
                        match err {
                            Vis::Pub => "adding 'pub'",
                            Vis::None => "removing the 'priv'",
                            Vis::Priv => ":O",
                        }
                    },
                });
            }
        }
        Ok(item)
    }

    pub fn handle_scope_error(
        err: ScopeError,
        span: Span,
        file: Ident,
        id: Ident,
        report: ReportSig,
        expected: &str,
        packages: &Packages,
        interner: &mut Interner,
        scope: &Scope,
        workspace: &mut Workspace,
    ) {
        let diag = match err {
            ScopeError::NotFound => diag! {
                (span, file) => "{} not found (queried: {})" { expected, &interner[id] },
                //(none) => "maybe you meant {}" { ??? } // TODO
            },
            ScopeError::Collision => diag! {
                (span, file) => "the identifier is ambiguous",
                (none) => "hint: specify the module from which it is imported ({})" {
                    {
                        if let Some(module) = packages.modules.get(file) {
                            packages.conns[module.deps]
                                .iter()
                                .map(|dep| interner.intern(scoped_ident!(packages.span_str(file, dep.name), id)))
                                .filter_map(|id| scope.get(id).is_ok().then_some(id))
                                .collect::<Vec<_>>() // borrow checker would complain, rightfully so
                                .into_iter()
                                .map(|id| &interner[id])
                                .collect::<Vec<_>>()
                                .join(", ")
                        } else {
                            "wait what?".to_string()
                        }
                    }
                },
            },
            ScopeError::TypeMismatch(id) => diag!(
                (span, file) => "the identifier exists but it's not of a correct kind",
                (none) => "found item was of kind '{}' but context required '{}'" {
                    report(id), expected.split('|').collect::<Vec<_>>().join("' | '")
                },
            ),
        };
        workspace.push(diag);
    }

    macro_rules! make_ty_report {
        (
            $(
                $name:ident {
                    $($ty:ty => $report:expr,)*
                }
            ),*
        ) => {
            $(
                pub fn $name(id: TypeId) -> String {
                    match id {
                        $(
                            const { TypeId::of::<$ty>() } => $report.to_string(),
                        )*
                        _ => "unknown".to_string(),
                    }
                }
            )*
        };
    }

    pub type ReportSig = fn(TypeId) -> String;
    pub struct Reports;

    impl Reports {
        make_ty_report!(
            base {
                Def => "function",
                BoundFunc => "bound function",
                Ty => "type",
            }
        );
    }
}
