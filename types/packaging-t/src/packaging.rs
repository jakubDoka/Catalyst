use lexing_t::*;
use scope::*;
use std::{any::Any, path::*};
use storage::*;

pub type PackageGraph = graphs::ProjectedCycleDetector;

#[derive(Default)]
pub struct Packages {
    pub modules: Map<VRef<str>, Mod>,
    pub conns: CacheBumpMap<Dep>,
    pub module_order: Vec<VRef<str>>,
}

impl Packages {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn ident_as_mod(&self, ident: VRef<str>) -> Option<VRef<Mod>> {
        self.modules.get(&ident)?;
        Some(unsafe { ident.cast() })
    }

    pub fn mod_as_ident(&self, module: VRef<Mod>) -> VRef<str> {
        unsafe { module.cast() }
    }

    pub fn reveal_span_lines(&self, file: VRef<str>, span: Span) -> Option<Span> {
        self.modules
            .get(&file)
            .map(|package| package.reveal_span_lines(span))
    }

    pub fn span_str(&self, file: VRef<str>, span: Span) -> &str {
        self.modules
            .get(&file)
            .map(|file| file.span_str(span))
            .unwrap_or_default()
    }

    pub fn check_vis(
        &self,
        item_module: VRef<str>,
        module: VRef<str>,
        vis: Vis,
    ) -> Result<(), Vis> {
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

    pub fn reveal_span_lines(&self, span: Span) -> Span {
        span.reveal_lines(&self.content)
    }

    pub fn is_module(&self) -> bool {
        self.kind.is_module()
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

#[derive(Clone, Debug)]
pub enum ModKind {
    Package {
        root_module: PathBuf,
        span: Maybe<Span>,
    },
    Module {
        package: VRef<str>,
        ordering: usize,
        items: Vec<ModItem>,
    },
    Default,
}

impl ModKind {
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module { .. })
    }
}

impl Default for ModKind {
    fn default() -> Self {
        ModKind::Default
    }
}

#[derive(Default, Clone, Copy)]
pub struct Dep {
    pub name_span: Span,
    pub name: VRef<str>,
    pub ptr: VRef<str>,
}

#[derive(Default, Clone, Copy)]
pub struct ModId(pub VRef<str>);
