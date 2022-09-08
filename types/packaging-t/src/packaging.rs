use lexing_t::*;
use scope::*;
use std::{any::Any, path::*};
use storage::*;

pub type PackageGraph = graphs::ProjectedCycleDetector;

#[derive(Default)]
pub struct Packages {
    pub modules: Map<Ident, Mod>,
    pub conns: CacheBumpMap<Dep>,
    pub module_order: Vec<Ident>,
}

impl Packages {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reveal_span_lines(&self, file: Ident, span: Span) -> Span {
        self.modules.get(&file).unwrap().reveal_span_lines(span)
    }

    pub fn span_str(&self, file: Ident, span: Span) -> &str {
        self.modules
            .get(&file)
            .map(|file| file.span_str(span))
            .unwrap_or_default()
    }

    pub fn check_vis(&self, item_module: Ident, module: Ident, vis: Vis) -> Result<(), Vis> {
        match vis {
            Vis::Pub => Ok(()),
            _ if item_module == module => Ok(()),
            kind => match (self.modules.get(&module), self.modules.get(&item_module)) {
                (
                    Some(Mod {
                        kind: ModKind::Module { package, .. },
                        ..
                    }),
                    Some(Mod {
                        kind:
                            ModKind::Module {
                                package: item_package,
                                ..
                            },
                        ..
                    }),
                ) => (package == item_package)
                    .then_some(())
                    .ok_or((kind == Vis::None).then_some(Vis::Pub).unwrap_or(Vis::None)),
                (_, None) => Ok(()),
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Default, Debug)]
pub struct Mod {
    pub path: PathBuf,
    pub deps: VSlice<Dep>,
    pub content: String,
    pub kind: ModKind,
    pub line_mapping: LineMapping,
}

impl Mod {
    pub fn span_str(&self, span: Span) -> &str {
        &self.content[span.start as usize..span.end as usize]
    }

    pub fn add_item(&mut self, item: ModItem) {
        if let ModKind::Module { ref mut items, .. } = self.kind {
            items.push(item);
        } else {
            unreachable!()
        }
    }

    fn reveal_span_lines(&self, span: Span) -> Span {
        span.reveal_lines(&self.content)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ModItem {
    pub id: Ident,
    pub ptr: ScopePtr,
    pub span: Span,
    pub whole_span: Span,
    pub vis: Vis,
}

impl ModItem {
    pub fn new(id: Ident, ptr: VRef<impl Any>, span: Span, whole_span: Span, vis: Vis) -> Self {
        Self {
            id,
            ptr: ScopePtr::new(ptr),
            span,
            whole_span,
            vis,
        }
    }

    pub fn to_scope_item(&self, module: Ident) -> ScopeItem {
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

#[derive(Clone, Debug)]
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

#[derive(Default, Clone, Copy)]
pub struct Dep {
    pub name: Span,
    pub ptr: Ident,
}
