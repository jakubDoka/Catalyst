#![feature(let_else)]
#![feature(iter_intersperse)]

#[macro_export]
macro_rules! span_str {
    ($self:expr, $span:expr) => {
        $self.packages.span_str($self.current_file, $span)
    };
}

mod packaging;
mod resources;

pub use packaging::{Dep, Mod, ModItem, ModKind, PackageGraph, Packages};

pub const MANIFEST_EXTENSION: &str = "ctlm";
pub const FILE_EXTENSION: &str = "ctl";
pub const DEP_ROOT_VAR: &str = "CATALYST_DEP_ROOT";
pub const DEFAULT_DEP_ROOT: &str = "deps";
