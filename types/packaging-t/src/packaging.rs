use lexing_t::*;
use rkyv::{
    with::{AsString, Skip, UnixTimestamp},
    Archive, Deserialize, Serialize,
};

use std::{default::default, io, path::*, time::SystemTime};
use storage::*;

use crate::ResourceDb;

pub type PackageGraph = graphs::CycleDetector;

const BUILTIN_PACKAGE_SOURCE: &str = include_str!("water_drops.ctl");

#[derive(Serialize, Deserialize, Archive)]

pub struct Resources {
    pub sources: PoolMap<Source>,
    #[with(Skip)]
    pub packages: PushMap<Package>,
    #[with(Skip)]
    pub modules: PushMap<Module>,
    #[with(Skip)]
    pub package_deps: PushMap<Dep<Package>>,
    #[with(Skip)]
    pub module_deps: PushMap<Dep<Module>>,
    #[with(Skip)]
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
            loaded: true,
            builtin: true,
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

    pub fn span_str(&self, source: VRef<Source>, span: Span) -> &str {
        self.sources[source].span_str(span)
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

#[derive(Archive, Serialize, Deserialize, Debug)]

pub struct Source {
    #[with(AsString)]
    pub path: PathBuf,
    #[with(UnixTimestamp)]
    pub last_modified: SystemTime,
    #[with(Skip)]
    pub content: String,
    #[with(Skip)]
    pub line_mapping: LineMapping,
    pub changed: bool,
    pub dead: bool,
    #[with(Skip)]
    pub loaded: bool,
    pub builtin: bool,
}

impl Source {
    pub fn span_str(&self, span: Span) -> &str {
        &self.content[span.start as usize..span.end as usize]
    }

    pub fn reveal_span_lines(&self, span: Span) -> Span {
        span.reveal_lines(&self.content)
    }

    pub fn ensure_loaded(&mut self, resources: &mut dyn ResourceDb) -> io::Result<()> {
        if self.loaded {
            return Ok(());
        }

        self.content = if self.builtin {
            BUILTIN_PACKAGE_SOURCE.to_string()
        } else {
            resources.read_to_string(&self.path)?
        };
        self.line_mapping = LineMapping::new(&self.content);
        self.loaded = true;

        Ok(())
    }
}

#[derive(Clone)]
pub struct Package {
    pub root_module: PathBuf,
    pub root_module_span: Span,
    pub deps: VSlice<Dep<Package>>,
    pub source: VRef<Source>,
    pub is_external: bool,
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
