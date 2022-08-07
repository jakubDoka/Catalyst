#[macro_export]
macro_rules! span_str {
    ($self:expr, $span:expr) => {
        $self.packages.span_str($self.current_file, $span)
    };
}

#[macro_export]
macro_rules! insert_scope_item {
    () => {
        pub fn insert_scope_item(&mut self, item: ModItem) {
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

#[macro_export]
macro_rules! duplicate_definition {
    () => {
        fn duplicate_definition(
            &mut self,
            duplicate: lexing_t::Span,
            because: impl Into<Maybe<lexing_t::Span>>,
        ) {
            $crate::duplicate_definition(
                duplicate,
                because.into(),
                self.current_file,
                &mut self.workspace,
            );
        }
    };
}

mod packaging;

pub use packaging::{Dep, DepList, Mod, ModItem, ModKind, PackageGraph, Packages};

pub use util::{duplicate_definition, insert_scope_item};

mod util {
    use crate::*;
    use diags::*;
    use inner_lexing::*;
    use scope::*;
    use storage::*;

    pub fn insert_scope_item(
        item: ModItem,
        scope: &mut Scope,
        current_file: Ident,
        interner: &mut Interner,
        packages: &mut Packages,
        workspace: &mut Workspace,
    ) {
        if let Err(span) = scope.insert(current_file, item.to_scope_item(current_file), interner) {
            duplicate_definition(item.span, span, current_file, workspace);
        }

        packages
            .modules
            .get_mut(current_file)
            .unwrap()
            .add_item(item);
    }

    pub fn duplicate_definition(
        duplicate: Span,
        because: Maybe<Span>,
        current_file: Ident,
        workspace: &mut Workspace,
    ) {
        workspace.push(diag! {
            (duplicate, current_file) error => "duplicate definition",
            (because, current_file) => "previous definition",
        })
    }
}
