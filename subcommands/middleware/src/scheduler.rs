use std::path::*;

use diags::*;
use packaging::*;
use packaging_t::*;
use storage::*;

#[derive(Default)]
pub struct Scheduler {
    pub interner: Interner,
    pub workspace: Workspace,
    pub resources: Resources,
    pub package_graph: PackageGraph,
    pub resource_loading_ctx: PackageLoaderCtx,
}

impl Scheduler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reload_resources(&mut self, path: &Path) {
        package_loader!(self).reload(path, &mut self.resource_loading_ctx);
    }
}
