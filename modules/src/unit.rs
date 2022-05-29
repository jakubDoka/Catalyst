use module_types::units::Units;
use std::path::Path;
use std::process::Command;
use std::{collections::VecDeque, path::PathBuf};

use crate::manifest::*;
use crate::module::*;
use ast::*;
use lexer::*;
use module_types::{error::ModuleError, *};
use parser::*;
use storage::*;

pub struct UnitBuilder<'a> {
    pub sources: &'a mut Sources,
    pub units: &'a mut Units,
    pub ctx: &'a mut LoaderContext,
    pub diagnostics: &'a mut errors::Diagnostics,
}

#[macro_export]
macro_rules! unit_builder {
    ($self:expr) => {
        UnitBuilder::new(
            &mut $self.sources,
            &mut $self.units,
            &mut $self.loader_context,
            &mut $self.diagnostics,
        )
    };
}

impl<'a> UnitBuilder<'a> {
    pub fn new(
        sources: &'a mut Sources,
        units: &'a mut Units,
        ctx: &'a mut LoaderContext,
        diagnostics: &'a mut errors::Diagnostics,
    ) -> Self {
        Self {
            sources,
            units,
            ctx,
            diagnostics,
        }
    }

    pub fn load_units(&mut self, root: &Path) -> errors::Result<Vec<Unit>> {
        self.ctx.clear();

        let root = root.canonicalize().map_err(|err| {
            self.diagnostics.push(ModuleError::RootUnitNotFound {
                path: root.to_path_buf(),
                trace: err,
            });
        })?;

        let slot = self.units.push(units::UnitEnt::new());
        self.ctx.map.insert(root.as_path(), slot);

        self.ctx.unit_frontier.push_back((root, None, slot));

        while let Some((path, span, slot)) = self.ctx.unit_frontier.pop_front() {
            self.ctx.buffer.clear();
            self.ctx.buffer.push(&path);
            self.ctx.buffer.push(MANIFEST_LOCAL_PATH);
            let Ok(contents) = std::fs::read_to_string(&self.ctx.buffer).map_err(|err| {
                self.diagnostics.push(ModuleError::ManifestLoadFail {
                    path: path.clone(),
                    trace: err,
                    loc: span,
                })
            }) else {
                self.ctx.cycle_graph.close_node(0);
                if self.ctx.unit_frontier.is_empty() {
                    return Err(());
                }
                continue;
            };

            {
                // all parsing components assume source code is registered in sources
                let source = SourceEnt::new(self.ctx.buffer.clone(), contents);
                let source = self.sources.push(source);
                self.units[slot].source = source;

                self.ctx.ast.clear();
                Parser::parse_manifest(
                    self.sources,
                    self.diagnostics,
                    &mut self.ctx.ast,
                    &mut self.ctx.ast_temp,
                    source,
                );
            }

            let manifest = Manifest::new(&self.ctx.ast, self.sources);

            if let Some(dependency) = manifest.dependencies() {
                for ManifestDepInfo {
                    name,
                    path: path_span,
                    version,
                } in dependency
                {
                    let path_str = self.sources.display(path_span);
                    let path = if path_str.starts_with(GITHUB_DOMAIN) {
                        let path = Path::join(&self.ctx.mf_root, path_str);
                        if !path.exists() {
                            if self
                                .download_git_repo(
                                    &path,
                                    path_str,
                                    self.sources.display(version),
                                    path_span,
                                )
                                .map_err(|err| self.diagnostics.push(err))
                                .is_err()
                            {
                                continue;
                            }
                        }
                        path
                    } else {
                        Path::join(&path, path_str)
                    };

                    let Ok(path) = path.canonicalize().map_err(|err| {
                        self.diagnostics.push(ModuleError::UnitNotFound {
                            path: path.to_path_buf(),
                            trace: err,
                            loc: path_span,
                        });
                    }) else {
                        continue;
                    };

                    let id = if let Some(&id) = self.ctx.map.get(path.as_path()) {
                        id
                    } else {
                        let id = self.units.push(units::UnitEnt::new());
                        self.ctx.map.insert(path.as_path(), id);
                        self.ctx
                            .unit_frontier
                            .push_back((path, Some(path_span), id));
                        id
                    };

                    self.ctx.map.insert((self.sources.display(name), slot), id);
                    self.ctx.cycle_graph.add_edge(id.0);
                }
            }

            let root_path_str = manifest
                .get_string_tag("root")
                .unwrap_or(DEFAULT_ROOT_SOURCE_PATH);
            // we preserve already loaded path segments
            self.units[slot].local_source_path = PathBuf::from(root_path_str);
            self.units[slot].root_path = path;

            self.ctx.cycle_graph.close_node(0);
        }

        let mut ordering = Vec::with_capacity(TreeStorage::<Unit>::max_node(&self.ctx.cycle_graph));
        self.ctx
            .cycle_graph
            .detect_cycles(slot, Some(&mut ordering))
            .map_err(|err| self.diagnostics.push(ModuleError::UnitCycle { cycle: err }))?;

        Ok(ordering)
    }

    fn download_git_repo(
        &self,
        path: &Path,
        link: &str,
        version: &str,
        span: Span,
    ) -> Result<(), ModuleError> {
        std::fs::create_dir_all(path).map_err(|err| ModuleError::MkGirtDir {
            path: path.to_path_buf(),
            trace: err,
            loc: span,
        })?;

        let status = Command::new("git")
            .arg("clone")
            .arg("--depth=1")
            .arg("--branch")
            .arg(version)
            .arg(link)
            .arg(path)
            .status()
            .map_err(|err| ModuleError::GitCloneExec {
                trace: err,
                loc: span,
            })?;

        status
            .success()
            .then_some(())
            .ok_or_else(|| ModuleError::GitCloneStatus {
                code: status,
                loc: span,
            })?;

        Ok(())
    }
}

pub struct LoaderContext {
    pub mf_root: PathBuf,
    pub cycle_graph: GenericGraph,
    pub buffer: PathBuf,
    pub unit_frontier: VecDeque<(PathBuf, Option<Span>, Unit)>,
    pub module_frontier: VecDeque<(PathBuf, Span, Source)>,
    pub map: Map<Unit>,
    pub ast: AstData,
    pub ast_temp: FramedStack<Ast>,
}

impl LoaderContext {
    pub fn new() -> Self {
        Self {
            mf_root: PathBuf::from(
                std::env::var(RESOURCE_ROOT_VAR)
                    .unwrap_or_else(|_| DEFAULT_RESOURCE_ROOT_VAR.to_string()),
            ),
            cycle_graph: GenericGraph::new(),
            buffer: PathBuf::new(),
            unit_frontier: VecDeque::new(),
            module_frontier: VecDeque::new(),
            map: Map::new(),
            ast: AstData::new(),
            ast_temp: FramedStack::new(),
        }
    }

    pub fn clear(&mut self) {
        self.cycle_graph.clear();
        self.unit_frontier.clear();
        self.map.clear();
        self.ast.clear();
    }
}
