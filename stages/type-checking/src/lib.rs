#![feature(let_else)]

#[macro_export]
macro_rules! scope_error_handler {
    ($self:expr, $span:expr, $id:expr, $message:expr) => {
        |err| {
            handle_scope_error(
                err,
                $span,
                $self.current_file,
                $id,
                $message,
                $self.packages,
                $self.interner,
                $self.scope,
                $self.workspace,
            )
        }
    };
}

mod item_collector;
mod state_gen;
mod ty_builder;
mod ty_parser;

pub use state_gen::{ItemCollector, TyBuilder, TyParser};
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
                        let deps = packages.modules[file].deps;
                        packages.conns[deps]
                            .iter()
                            .map(|dep| interner.intern(scoped_ident!(dep.ptr, id)))
                            .filter_map(|id| scope.get(id).is_ok().then_some(id))
                            .collect::<Vec<_>>() // borrow checker would complain, rightfully so
                            .into_iter()
                            .map(|id| &interner[id])
                            .collect::<Vec<_>>()
                            .join(", ")
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