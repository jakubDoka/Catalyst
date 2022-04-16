use cranelift_entity::PrimaryMap;
use std::path::Path;
use std::process::Command;
use std::{collections::VecDeque, path::PathBuf};

use crate::*;
use lexer::*;
use parser::*;

use crate::{error, Error};

pub type Units = PrimaryMap<Unit, Ent>;

pub struct Loader<'a> {
    pub sources: &'a mut Sources,
    pub units: &'a mut Units,
    pub ctx: &'a mut LoaderContext,
}

impl<'a> Loader<'a> {
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

#[derive(Debug, Default)]
pub struct Ent {
    pub local_source_path: PathBuf,
    pub root_path: PathBuf,
    pub source: Source,
}

impl Ent {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_absolute_source_path(&self) -> std::io::Result<PathBuf> {
        self.root_path
            .join(self.local_source_path.as_path())
            .canonicalize()
    }
}

lexer::gen_entity!(Unit);

pub struct LoaderContext {
    pub mf_root: PathBuf,
    pub graph: GenericGraph,
    pub buffer: PathBuf,
    pub frontier: VecDeque<(PathBuf, Span, Unit)>,
    pub map: Map<Unit>,
    pub ast: ast::Data,
    pub ast_temp: ast::Temp,
}

impl LoaderContext {
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

    pub fn clear(&mut self) {
        self.graph.clear();
        self.frontier.clear();
        self.map.clear();
        self.ast.clear();
    }
}
