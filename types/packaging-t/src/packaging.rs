use lexing_t::*;
use serde::{Deserialize, Serialize};

use std::{default::default, path::*, time::SystemTime};
use storage::*;

pub type PackageGraph = graphs::CycleDetector;

const BUILTIN_PACKAGE_SOURCE: &str = include_str!("water_drops.ctl");

#[derive(Serialize, Deserialize)]
pub struct Resources {
    pub sources: PoolMap<Source>,
    #[serde(skip)]
    pub packages: PushMap<Package>,
    #[serde(skip)]
    pub modules: PushMap<Module>,
    #[serde(skip)]
    pub package_deps: PushMap<Dep<Package>>,
    #[serde(skip)]
    pub module_deps: PushMap<Dep<Module>>,
    #[serde(skip)]
    pub module_order: Vec<VRef<Module>>,
}

impl Default for Resources {
    fn default() -> Self {
        Self::new()
    }
}

impl Resources {
    pub const BUILTIN_PACKAGE: VRef<Package> = VRef::new(0);
    pub const BUILTIN_SOURCE: VRef<Source> = VRef::new(0);
    pub const BUILTIN_SOURCE_PATH: &str = "compiler/builtin.ctl";

    pub fn new() -> Self {
        let mut s = Self {
            sources: default(),
            packages: default(),
            modules: default(),
            package_deps: default(),
            module_deps: default(),
            module_order: default(),
        };

        let builtin_source = s.sources.push(Source {
            path: Self::BUILTIN_SOURCE_PATH.into(),
            last_modified: SystemTime::UNIX_EPOCH,
            content: BUILTIN_PACKAGE_SOURCE.into(),
            line_mapping: LineMapping::new(BUILTIN_PACKAGE_SOURCE),
            changed: true,
            dead: false,
        });
        assert_eq!(Self::BUILTIN_SOURCE, builtin_source);

        s.clear();

        s
    }

    pub fn is_external(&self, module: VRef<Module>) -> bool {
        self.packages[self.modules[module].package].is_external
    }

    pub fn no_changes(&self) -> bool {
        self.sources.values().all(|s| !s.changed)
    }

    pub fn source_path(&self, source: VRef<Source>) -> &Path {
        self.sources[source].path.as_path()
    }

    pub fn clear(&mut self) {
        self.sources
            .values_mut()
            .skip(1) // skip builtin source
            .for_each(|s| s.dead = true); // to avoid io
        self.packages.clear();
        self.modules.clear();
        self.package_deps.clear();
        self.module_deps.clear();
        self.module_order.clear();

        let builtin_package = self.packages.push(Package {
            root_module: Self::BUILTIN_SOURCE_PATH.into(),
            root_module_span: default(),
            deps: default(),
            source: Self::BUILTIN_SOURCE,
            is_external: true,
        });
        assert_eq!(Self::BUILTIN_PACKAGE, builtin_package);
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

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Deserialize, Serialize, Clone)]
pub struct Package {
    pub root_module: PathBuf,
    pub root_module_span: Span,
    pub deps: VSlice<Dep<Package>>,
    pub source: VRef<Source>,
    pub is_external: bool,
}

#[derive(Deserialize, Serialize, Clone, Copy, Debug)]
pub struct Module {
    pub package: VRef<Package>,
    pub ordering: usize,
    pub deps: VSlice<Dep<Module>>,
    pub source: VRef<Source>,
}

#[derive(Deserialize, Serialize)]
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
