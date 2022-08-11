#![feature(let_else)]
#![feature(let_chains)]
#![feature(default_free_fn)]
#![feature(decl_macro)]

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
            message: impl Into<String>,
        ) -> errors::Result<ScopePtr> {
            self.scope.get(id).map_err(|err| {
                crate::handle_scope_error(
                    err,
                    span,
                    self.current_file,
                    id,
                    message,
                    &self.packages,
                    &mut self.interner,
                    &self.scope,
                    &mut self.workspace,
                )
            })
        }
    };

    (concrete) => {
        pub fn get_from_scope_concrete<T: VPtr + 'static>(
            &mut self,
            id: Ident,
            span: lexing_t::Span,
            message: impl Into<String>,
        ) -> errors::Result<T> {
            self.scope.get_concrete::<T>(id).map_err(|err| {
                crate::handle_scope_error(
                    err,
                    span,
                    self.current_file,
                    id,
                    message,
                    &self.packages,
                    &mut self.interner,
                    &self.scope,
                    &mut self.workspace,
                )
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
pub use utils::handle_scope_error;

mod utils {
    use diags::*;
    use inner_lexing::*;
    use packaging_t::*;
    use scope::*;
    use storage::*;

    pub fn handle_scope_error(
        err: ScopeError,
        span: Span,
        file: Ident,
        id: Ident,
        message: impl Into<String>,
        packages: &Packages,
        interner: &mut Interner,
        scope: &Scope,
        workspace: &mut Workspace,
    ) {
        let diag = match err {
            ScopeError::NotFound => diag! {
                (span, file) => "{}" { message.into() },
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
            ScopeError::TypeMismatch => diag!(
                (span, file) => "the identifier is not a type",
            ),
        };
        workspace.push(diag);
    }
}
