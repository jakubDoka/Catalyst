use diags::DiagPackages;
use lexing_t::*;
use scope::*;
use std::path::*;
use storage::*;

pub type PackageGraph = graphs::ProjectedCycleDetector;

#[derive(Default)]
pub struct Packages {
    pub modules: Map<Mod>,
    pub conns: CacheBumpMap<DepList, Dep>,
    pub module_order: Vec<Ident>,
}

impl Packages {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn span_str(&self, file: Ident, span: Span) -> &str {
        self.modules
            .get(file)
            .map(|file| file.span_str(span))
            .unwrap_or_default()
    }
}

impl DiagPackages for Packages {
    fn line_info(&self, module: Ident, pos: Option<usize>) -> (&Path, Option<(usize, usize)>) {
        let module = self.modules.get(module).unwrap();
        (
            module.path.as_path(),
            pos.map(|pos| module.line_mapping.line_info_at(pos)),
        )
    }

    fn content_of(&self, module: Ident) -> &str {
        self.modules.get(module).unwrap().content.as_str()
    }
}

#[derive(Default)]
pub struct Mod {
    pub path: PathBuf,
    pub deps: Maybe<DepList>,
    pub content: String,
    pub kind: ModKind,
    pub line_mapping: LineMapping,
}

impl Mod {
    pub fn span_str(&self, span: Span) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(
                &self.content.as_bytes()[span.start as usize..span.end as usize],
            )
        }
    }

    pub fn add_item(&mut self, item: ModItem) {
        if let ModKind::Module { ref mut items, .. } = self.kind {
            items.push(item);
        } else {
            unreachable!()
        }
    }
}

pub struct ModItem {
    pub id: Ident,
    pub ptr: ScopePtr,
    pub span: Span,
}

impl ModItem {
    pub fn new(id: Ident, ptr: impl VPtr + 'static, span: Span) -> Self {
        Self {
            id,
            ptr: ScopePtr::new(ptr),
            span,
        }
    }

    pub fn to_scope_item(&self, module: Ident) -> ScopeItem {
        ScopeItem {
            id: self.id,
            ptr: self.ptr,
            span: self.span,
            module,
        }
    }
}

pub enum ModKind {
    Package {
        root_module: PathBuf,
        span: Maybe<Span>,
    },
    Module {
        package: Ident,
        ordering: usize,
        items: Vec<ModItem>,
    },
    Default,
}

impl Default for ModKind {
    fn default() -> Self {
        ModKind::Default
    }
}

pub struct Dep {
    pub name: Span,
    pub ptr: Ident,
}

gen_v_ptr!(DepList);
