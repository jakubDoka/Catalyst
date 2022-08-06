#[macro_export]
macro_rules! span_str {
    ($self:expr, $span:expr) => {
        $self.packages.span_str($self.current_file, $span)
    };
}

#[macro_export]
macro_rules! insert_scope_item {
    ($self:expr, $res:expr) => {
        if let Err(span) = $self.scope.insert(
            $self.current_file,
            $res.to_scope_item($self.current_file),
            $self.interner,
        ) {
            $crate::duplicate_definition!($self, $res.span, span);
        }
        $self
            .packages
            .modules
            .get_mut($self.current_file)
            .unwrap()
            .add_item($res);
    };
}

#[macro_export]
macro_rules! duplicate_definition {
    ($self:expr, $a:expr, $b:expr) => {
        $self.workspace.push(diags::diag! {
            ($a, $self.current_file) error => "duplicate definition",
            ($b, $self.current_file) => "previous definition",
        })
    };
}

mod packaging;

pub use packaging::{Dep, DepList, Mod, ModItem, ModKind, PackageGraph, Packages};
