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
            $self.workspace.push(diags::diag! {
                ($res.span, $self.current_file) error => "duplicate definition",
                (span, $self.current_file) => "previous definition",
            })
        }
        $self.packages.modules.get_mut($self.current_file).unwrap().add_item($res);
    };
}

mod packaging;

pub use packaging::{Dep, DepList, Mod, ModItem, ModKind, PackageGraph, Packages};
