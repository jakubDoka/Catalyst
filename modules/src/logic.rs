use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    process::Command,
};

use cranelift_entity::PrimaryMap;
use lexer::{
    map::Map,
    SourcesExt, {SourceEnt, Sources, Span},
};
use parser::{
    ast, {Convert, Parser},
};

use crate::{
    error::{self, Error},
    manifest::{Manifest, ManifestDepInfo},
    module::{self, Module, ModuleImport, ModuleImports},
    tree::{GenericGraph, TreeStorage},
    unit::{self, Unit},
};

type Result<T = ()> = std::result::Result<T, Error>;

pub type Modules = PrimaryMap<Module, module::Ent>;
pub type Units = PrimaryMap<Unit, unit::Ent>;

pub const SOURCE_FILE_EXTENSION: &'static str = "mf";
pub const MANIFEST_FILE_EXTENSION: &'static str = "mfm";
pub const RESOURCE_ROOT_VAR: &'static str = "MF_ROOT";
pub const DEFAULT_RESOURCE_ROOT_VAR: &'static str = ".mp_build_resources";
pub const MANIFEST_LOCAL_PATH: &'static str = "project.mfm";
pub const GITHUB_DOMAIN: &'static str = "github.com";
pub const DEFAULT_ROOT_SOURCE_PATH: &'static str = "src/root.mf";

pub struct ModuleLoader<'a> {
    pub sources: &'a mut Sources,
    pub modules: &'a mut Modules,
    pub units: &'a mut Units,
    pub frontier: &'a mut VecDeque<(PathBuf, Span, Module)>,
    pub ctx: &'a mut UnitLoaderContext,
    pub map: &'a mut Map<Module>,
}

impl<'a> ModuleLoader<'a> {
    pub fn load_unit_modules(&mut self, unit: Unit) -> Result<Vec<Module>> {
        self.ctx.clear();
        let unit_ent = &self.units[unit];
        let base_line = self.modules.len() as u32;

        let Ok(path) = unit_ent.get_absolute_source_path() else {
            return Err(error::Error::spanless(error::Kind::ModuleNotFound(
                Path::join(&unit_ent.root_path, &unit_ent.local_source_path)
            )));
        };

        let id = path.as_path().into();
        let module = self.modules.push(module::Ent::new(id));
        self.map.insert(id, module);

        self.frontier.push_back((path, Span::default(), module));

        while let Some((path, span, slot)) = self.frontier.pop_front() {
            let content = std::fs::read_to_string(&path).map_err(|err| {
                error::Error::new(
                    error::Kind::ModuleLoadFailed(path.clone(), err), // todo: save module path
                    span,
                )
            })?;

            let source = SourceEnt::new(path, content);
            let source = self.sources.push(source);
            let content = self.sources[source].content();
            self.modules[slot].source = source;

            self.ctx.ast.clear();
            Parser::parse_imports(content, &mut self.ctx.ast, &mut self.ctx.ast_temp, source)
                .map_err(Convert::convert)?;

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

                    let Ok(path) = self.ctx.buffer.canonicalize() else {
                        return Err(error::Error::new(error::Kind::ModuleNotFound(self.ctx.buffer.clone()), path_span));
                    };

                    let id = path.as_path().into();

                    let id = if let Some(&id) = self.map.get(id) {
                        id
                    } else {
                        let module = self.modules.push(module::Ent::new(id));
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
        if let Some(mut cycle) = self.ctx.graph.detect_cycles(Module(0), Some(&mut ordering)) {
            cycle.iter_mut().for_each(|id| id.0 += base_line);
            return Err(Error::spanless(error::Kind::ModuleCycle(cycle)));
        }

        Ok(ordering)
    }
}

pub struct UnitLoaderContext {
    mf_root: PathBuf,
    graph: GenericGraph,
    buffer: PathBuf,
    frontier: VecDeque<(PathBuf, Span, Unit)>,
    map: Map<Unit>,
    ast: ast::Data,
    ast_temp: ast::Temp,
}

impl UnitLoaderContext {
    pub fn new() -> Self {
        Self {
            mf_root: PathBuf::from(
                std::env::var(RESOURCE_ROOT_VAR)
                    .unwrap_or_else(|_| DEFAULT_RESOURCE_ROOT_VAR.to_string()),
            ),
            graph: GenericGraph::new(),
            buffer: PathBuf::new(),
            frontier: VecDeque::new(),
            map: Map::new(),
            ast: ast::Data::new(),
            ast_temp: ast::Temp::new(),
        }
    }

    fn clear(&mut self) {
        self.graph.clear();
        self.frontier.clear();
        self.map.clear();
        self.ast.clear();
    }
}

pub struct UnitLoader<'a> {
    pub sources: &'a mut Sources,
    pub units: &'a mut Units,
    pub ctx: &'a mut UnitLoaderContext,
}

impl<'a> UnitLoader<'a> {
    pub fn load_units(&mut self, root: &Path) -> Result<Vec<Unit>> {
        self.ctx.clear();

        let Ok(root) = root.canonicalize() else {
            return Err(Error::new(error::Kind::UnitNotFound(root.to_owned()), Span::default()));
        };

        let slot = self.units.push(unit::Ent::new());
        self.ctx.map.insert(root.as_path(), slot);

        self.ctx.frontier.push_back((root, Span::default(), slot));

        while let Some((path, span, slot)) = self.ctx.frontier.pop_front() {
            self.ctx.buffer.clear();
            self.ctx.buffer.push(&path);
            self.ctx.buffer.push(MANIFEST_LOCAL_PATH);
            let contents = std::fs::read_to_string(&self.ctx.buffer).map_err(|err| {
                Error::new(
                    error::Kind::UnitLoadFailed(self.ctx.buffer.clone(), err),
                    span,
                )
            })?;

            {
                // all parsing components assume source code is registered in sources
                let source = SourceEnt::new(self.ctx.buffer.clone(), contents);
                let source = self.sources.push(source);
                let contents = self.sources[source].content();

                self.units[slot].source = source;

                self.ctx.ast.clear();
                Parser::parse_manifest(contents, &mut self.ctx.ast, &mut self.ctx.ast_temp, source)
                    .map_err(Convert::convert)?;
            }

            let manifest = Manifest::new(&self.ctx.ast, &self.sources);

            if let Some(dependency) = manifest.dependencies() {
                for ManifestDepInfo {
                    name,
                    path: span_path,
                    version,
                } in dependency
                {
                    let path_str = self.sources.display(span_path);
                    let path = if path_str.starts_with(GITHUB_DOMAIN) {
                        let path = Path::join(&self.ctx.mf_root, path_str);
                        if !path.exists() {
                            Self::download_git_repo(
                                &path,
                                path_str,
                                self.sources.display(version),
                                span_path,
                            )?;
                        }
                        path
                    } else {
                        Path::join(&path, path_str)
                    };

                    let Ok(path) = path.canonicalize() else {
                        return Err(Error::new(error::Kind::UnitNotFound(path), span));
                    };

                    let id = if let Some(&id) = self.ctx.map.get(path.as_path()) {
                        id
                    } else {
                        let id = self.units.push(unit::Ent::new());
                        self.ctx.map.insert(path.as_path(), id);
                        self.ctx.frontier.push_back((path, span_path, id));
                        id
                    };

                    self.ctx.map.insert((self.sources.display(name), slot), id);
                    self.ctx.graph.add_edge(id.0);
                }
            }

            let root_path_str = manifest
                .get_string_tag("root")
                .unwrap_or(DEFAULT_ROOT_SOURCE_PATH);
            // we preserve already loaded path segments
            self.units[slot].local_source_path = PathBuf::from(root_path_str);
            self.units[slot].root_path = path;

            self.ctx.graph.close_node();
        }

        let mut ordering = Vec::with_capacity(TreeStorage::<Unit>::len(&self.ctx.graph));
        if let Some(cycle) = self.ctx.graph.detect_cycles(slot, Some(&mut ordering)) {
            return Err(Error::spanless(error::Kind::UnitCycle(cycle)));
        }

        Ok(ordering)
    }

    fn download_git_repo(path: &Path, link: &str, version: &str, span: Span) -> Result<()> {
        std::fs::create_dir_all(path).map_err(|err| Error::new(error::Kind::MkDir(err), span))?;

        let status = Command::new("git")
            .arg("clone")
            .arg("--depth=1")
            .arg("--branch")
            .arg(version)
            .arg(link)
            .arg(path)
            .status()
            .or_else(|err| Err(Error::new(error::Kind::GitClone(err), span)))?;

        if !status.success() {
            return Err(Error::spanless(error::Kind::GitCloneStatus(status)));
        }

        Ok(())
    }
}
