#[macro_export]
macro_rules! span_str {
    ($self:expr, $span:expr) => {
        $self.packages.span_str($self.current_file, $span)
    };
}

#[macro_export]
macro_rules! insert_scope_item {
    () => {
        pub fn insert_scope_item(&mut self, item: $crate::ModItem, whole_span: lexing_t::Span) {
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

mod packaging;

pub use packaging::{Dep, Mod, ModItem, ModKind, PackageGraph, Packages};

pub use util::{duplicate_definition, insert_scope_item};

mod util {
    use crate::*;
    use diags::*;
    use lexing_t::*;
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
