use lexing_t::*;

use std::{default::default, path::*, time::SystemTime};
use storage::*;

use crate::*;

pub type PackageGraph = graphs::CycleDetector;

#[derive(Default)]
pub struct Resources {
    pub sources: PoolMap<Source>,
    pub packages: PushMap<Package>,
    pub modules: PushMap<Module>,
    pub package_deps: PushMap<Dep<Package>>,
    pub module_deps: PushMap<Dep<Module>>,
    pub module_order: Vec<VRef<Module>>,
    pub db: Box<dyn ResourceDb>,
}

impl Resources {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn no_changes(&self) -> bool {
        self.sources.values().all(|s| !s.changed)
    }

    pub fn source_path(&self, source: VRef<Source>) -> &Path {
        self.sources[source].path.as_path()
    }

    pub fn with_resources(resources: impl ResourceDb) -> Self {
        Self {
            db: Box::new(resources),
            ..default()
        }
    }

    pub fn clear(&mut self) {
        self.sources.values_mut().for_each(|s| s.dead = true); // to avoid io
        self.packages.clear();
        self.modules.clear();
        self.package_deps.clear();
        self.module_deps.clear();
        self.module_order.clear();
    }

    pub fn mark_changed(&mut self) {
        for &elem in &self.module_order {
            let Module { deps, source, .. } = self.modules[elem];
            self.sources[source].changed = self.sources[source].changed
                || self.module_deps[deps]
                    .iter()
                    .any(|&dep| self.sources[self.modules[dep.ptr].source].changed);
        }
    }

    pub fn mark_subgraph(&self, roots: &[VRef<Module>], mark: &mut BitSet) {
        let mut stack = roots.to_bumpvec();
        while let Some(elem) = stack.pop() {
            if mark.insert(elem.index()) {
                let Module { deps, .. } = self.modules[elem];
                stack.extend(self.module_deps[deps].iter().map(|&dep| dep.ptr));
            }
        }
    }
}

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub last_modified: SystemTime,
    pub content: String,
    pub line_mapping: LineMapping,
    pub changed: bool,
    pub dead: bool,
}

impl Source {
    pub fn span_str(&self, span: Span) -> &str {
        &self.content[span.start as usize..span.end as usize]
    }

    pub fn reveal_span_lines(&self, span: Span) -> Span {
        span.reveal_lines(&self.content)
    }
}
#[derive(Clone)]
pub struct Package {
    pub root_module: PathBuf,
    pub root_module_span: Span,
    pub deps: VSlice<Dep<Package>>,
    pub source: VRef<Source>,
}

#[derive(Clone, Copy, Debug)]
pub struct Module {
    pub package: VRef<Package>,
    pub ordering: usize,
    pub deps: VSlice<Dep<Module>>,
    pub source: VRef<Source>,
}

pub struct Dep<T: ?Sized> {
    pub vis: Option<Vis>,
    pub name_span: Span,
    pub name: Ident,
    pub ptr: VRef<T>,
}

impl<T: ?Sized> Clone for Dep<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Dep<T> {}
