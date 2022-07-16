use lexing::*;
use std::path::*;
use storage::*;

pub type PackageGraph = graphs::CycleDetector;

#[derive(Default)]
pub struct PackagingContext {
    pub modules: SparseMap<Ident, Module>,
    pub conns: CacheBumpMap<DepList, Dep>,
    pub module_order: Vec<Ident>,
}

impl PackagingContext {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Default)]
pub struct Module {
    pub path: PathBuf,
    pub deps: Maybe<DepList>,
    pub content: String,
    pub kind: ModuleKind,
    pub line_mapping: LineMapping,
}

pub enum ModuleKind {
    Package { root_module: PathBuf },
    Module { package: Ident, ordering: usize },
    Default,
}

impl Default for ModuleKind {
    fn default() -> Self {
        ModuleKind::Default
    }
}

pub struct Dep {
    pub name: Span,
    pub ptr: Ident,
}

gen_v_ptr!(DepList);
