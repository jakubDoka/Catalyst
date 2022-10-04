use lexing_t::*;
use scope::*;
use std::{any::Any, path::*};
use storage::*;

use crate::*;

pub type PackageGraph = graphs::ProjectedCycleDetector;

#[derive(Default)]
pub struct Resources {
    pub sources: PushMap<Source>,
    pub packages: PushMap<Package>,
    pub modules: PushMap<Module>,
    pub package_deps: BumpMap<Dep<Package>>,
    pub module_deps: BumpMap<Dep<Module>>,
    pub module_order: Vec<VRef<str>>,
    pub db: Box<dyn ResourceDb>,
}

impl Resources {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_resources(resources: impl ResourceDb + 'static) -> Self {
        Self {
            db: Box::new(resources),
            ..Default::default()
        }
    }
}

#[derive(Default, Debug)]
pub struct Source {
    pub path: PathBuf,
    pub content: String,
    pub line_mapping: LineMapping,
}

impl Source {
    pub fn span_str(&self, span: Span) -> &str {
        &self.content[span.start as usize..span.end as usize]
    }

    pub fn reveal_span_lines(&self, span: Span) -> Span {
        span.reveal_lines(&self.content)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ModItem {
    pub id: VRef<str>,
    pub ptr: ScopePtr,
    pub span: Span,
    pub whole_span: Span,
    pub vis: Vis,
}

impl ModItem {
    pub fn new(id: VRef<str>, ptr: VRef<impl Any>, span: Span, whole_span: Span, vis: Vis) -> Self {
        Self {
            id,
            ptr: ScopePtr::new(ptr),
            span,
            whole_span,
            vis,
        }
    }

    pub fn to_scope_item(&self, module: VRef<str>) -> ScopeItem {
        ScopeItem {
            id: self.id,
            ptr: self.ptr,
            span: self.span,
            whole_span: self.whole_span,
            vis: self.vis,
            module,
        }
    }
}

#[derive(Clone)]
pub struct Package {
    pub root_module: PathBuf,
    pub span: Option<Span>,
    pub deps: VSlice<Dep<Package>>,
    pub source: VRef<Source>,
}

#[derive(Clone, Copy)]
pub struct Module {
    pub package: VRef<Package>,
    pub ordering: usize,
    pub deps: VSlice<Dep<Module>>,
    pub source: VRef<Source>,
}

pub struct Dep<T: ?Sized> {
    pub name_span: Span,
    pub name: VRef<str>,
    pub ptr: VRef<T>,
}

impl<T: ?Sized> Clone for Dep<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Dep<T> {}
