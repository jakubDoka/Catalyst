use std::{
    collections::VecDeque,
    path::PathBuf,
};

use cranelift_entity::{EntityRef, PrimaryMap};

use lexer::*;
use parser::*;
use crate::error::Error;
use crate::*;

pub type Modules = PrimaryMap<Module, Ent>;

pub const SOURCE_FILE_EXTENSION: &'static str = "mf";
pub const MANIFEST_FILE_EXTENSION: &'static str = "mfm";
pub const RESOURCE_ROOT_VAR: &'static str = "MF_ROOT";
pub const DEFAULT_RESOURCE_ROOT_VAR: &'static str = ".mp_build_resources";
pub const MANIFEST_LOCAL_PATH: &'static str = "project.mfm";
pub const GITHUB_DOMAIN: &'static str = "github.com";
pub const DEFAULT_ROOT_SOURCE_PATH: &'static str = "src/root.mf";

pub struct Loader<'a> {
    pub sources: &'a mut Sources,
    pub modules: &'a mut Modules,
    pub units: &'a mut Units,
    pub frontier: &'a mut VecDeque<(PathBuf, Span, Module)>,
    pub ctx: &'a mut LoaderContext,
    pub map: &'a mut Map<Module>,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Loader<'a> {
    pub fn load_unit_modules(&mut self, unit: Unit) -> errors::Result<Vec<Module>> {
        self.ctx.clear();
        let unit_ent = &self.units[unit];
        let base_line = self.modules.len() as u32;

        let path = unit_ent.get_absolute_source_path().map_err(|trace| {
            self.diagnostics.push(Error::RootModuleNotFound {
                unit,
                trace,
            });
        })?;

        let id = path.as_path().into();
        let module = self.modules.push(Ent::new(id));
        self.map.insert(id, module);

        self.frontier.push_back((path, Span::default(), module));

        while let Some((path, span, slot)) = self.frontier.pop_front() {
            let Ok(content) = std::fs::read_to_string(&path).map_err(|err| {
                self.diagnostics.push(Error::ModuleLoadFail {
                    path: path.clone(),
                    trace: err,
                    loc: span,
                });
            }) else { 
                continue; 
            };

            let source = SourceEnt::new(path, content);
            let source = self.sources.push(source);
            self.modules[slot].source = source;

            self.ctx.ast.clear();
            Parser::parse_imports(self.sources, self.diagnostics, &mut self.ctx.ast, &mut self.ctx.ast_temp, source);

            if let Some(imports) = ModuleImports::new(&self.ctx.ast, &self.sources).imports() {
                for ModuleImport {
                    nick,
                    name,
                    path: path_span,
                } in imports
                {
                    self.ctx.buffer.clear();
                    let unit = self
                        .ctx
                        .map
                        .get((self.sources.display(name), unit))
                        .copied()
                        .unwrap_or(unit);

                    self.ctx.buffer.push(&self.units[unit].root_path);
                    self.ctx.buffer.push(&self.units[unit].local_source_path);
                    self.ctx.buffer.set_extension("");
                    self.ctx.buffer.push(self.sources.display(path_span));
                    self.ctx.buffer.set_extension(SOURCE_FILE_EXTENSION);

                    let Ok(path) = self.ctx.buffer.canonicalize().map_err(|err| {
                        self.diagnostics.push(Error::ModuleNotFound {
                            trace: err,
                            loc: path_span,
                        });
                    }) else {
                        continue;
                    };

                    let id = path.as_path().into();

                    let id = if let Some(&id) = self.map.get(id) {
                        id
                    } else {
                        let module = self.modules.push(Ent::new(id));
                        self.map.insert(id, module);
                        self.frontier.push_back((path, path_span, module));
                        module
                    };

                    let name = nick.unwrap_or(name);
                    self.map.insert((self.sources.display(name), slot), id);
                    if id.0 >= base_line {
                        self.ctx.graph.add_edge(id.0 - base_line);
                    }
                }
            }
            self.ctx.graph.close_node();
        }

        let mut ordering = Vec::with_capacity(TreeStorage::<Module>::len(&self.ctx.graph));
        self.ctx.graph.detect_cycles(Module(0), Some(&mut ordering)).map_err(|mut err| {
            err
                .iter_mut()
                .for_each(|id| id.0 += base_line);
            self.diagnostics.push(Error::ModuleCycle {
                cycle: err,
            });
        })?;

        Ok(ordering)
    }
}

#[derive(Debug)]
pub struct Ent {
    pub id: ID,
    pub source: Source,
    pub items: Vec<Item>,
}

impl Ent {
    pub fn new(id: ID) -> Self {
        Self {
            id,
            source: Source::default(),
            items: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Item {
    pub id: ID,
    pub kind: scope::Pointer,
    pub span: Span,
}

impl Item {
    pub fn new(id: ID, kind: impl EntityRef + 'static, span: Span) -> Self {
        Self {
            id,
            kind: scope::Pointer::write(kind),
            span,
        }
    }

    pub fn to_scope_item(&self) -> scope::Item {
        scope::Item {
            span: self.span,
            pointer: self.kind,
        }
    }
}

lexer::gen_entity!(Module);

pub struct ModuleImports<'a> {
    ast_data: &'a ast::Data,
    sources: &'a Sources,
}

impl<'a> ModuleImports<'a> {
    pub fn new(ast_data: &'a ast::Data, sources: &'a Sources) -> Self {
        ModuleImports { ast_data, sources }
    }

    pub fn imports(&self) -> Option<impl Iterator<Item = ModuleImport> + 'a> {
        self.ast_data.elements().next().map(|(_, e)| {
            assert!(e.kind != ast::Kind::Import);
            self.ast_data.slice(e.children).iter().map(|&c| {
                let (nick, path) = match self.ast_data.children(c) {
                    &[nick, path] => (Some(self.ast_data.nodes[nick].span), path),
                    &[path] => (None, path),
                    _ => unreachable!(),
                };
                let path = self.ast_data.nodes[path].span.strip_sides();
                if let Some(split) = self.sources.display(path).find('/') {
                    ModuleImport {
                        nick,
                        name: path.slice(..split),
                        path: path.slice(split + 1..),
                    }
                } else {
                    ModuleImport {
                        nick,
                        name: path,
                        path: path.slice(path.len()..),
                    }
                }
            })
        })
    }
}

pub struct ModuleImport {
    pub nick: Option<Span>,
    pub name: Span,
    pub path: Span,
}
