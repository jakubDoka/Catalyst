#![feature(let_else)]
#![feature(iter_intersperse)]

#[macro_export]
macro_rules! span_str {
    ($self:expr, $span:expr) => {
        $self.resources.sources[$self.source].span_str($span)
    };
}

mod packaging;
mod resources;

pub use packaging::{Dep, Module, Package, PackageGraph, Resources, Source};
pub use resources::{OsResources, ResourceDb};

pub const MANIFEST_EXTENSION: &str = "ctlm";
pub const FILE_EXTENSION: &str = "ctl";
pub const DEP_ROOT_VAR: &str = "CATALYST_DEP_ROOT";
pub const DEFAULT_DEP_ROOT: &str = "deps";
