use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    process::Command,
};

use cranelift_entity::PrimaryMap;
use lexer::{
    map::Map,
    {SourceEnt, Sources, Span},
};
use parser::{
    ast,
    {Convert, Parser},
};

use crate::{
    error::{self, Error},
    manifest::{Manifest, ManifestDepInfo},
    module::{self, Module, ModuleImport, ModuleImports},
    tree::{GenericGraph, TreeStorage},
    unit::{self, Unit},
};

type Result<T = ()> = std::result::Result<T, Error>;

pub struct Modules {
    units: PrimaryMap<Unit, unit::Ent>,
    modules: PrimaryMap<Module, module::Ent>,
}

impl Modules {
    pub const SOURCE_FILE_EXTENSION: &'static str = "mf";
    pub const MANIFEST_FILE_EXTENSION: &'static str = "mfm";
    pub const RESOURCE_ROOT_VAR: &'static str = "MF_ROOT";
    pub const DEFAULT_RESOURCE_ROOT_VAR: &'static str = ".mp_build_resources";
    pub const MANIFEST_LOCAL_PATH: &'static str = "project.mfm";
    pub const GITHUB_DOMAIN: &'static str = "github.com";
    pub const DEFAULT_ROOT_SOURCE_PATH: &'static str = "src/root.mf";

    pub fn new() -> Self {
        Modules {
            units: PrimaryMap::new(),
            modules: PrimaryMap::new(),
        }
    }

    pub fn load(&mut self, sources: &mut Sources, root_path: &Path) -> Result<Vec<Vec<Module>>> {
        let mut unit_map = Map::new();
        let mut module_map = Map::new();

        let unit_order = self.load_units(sources, &mut unit_map, root_path)?;
        let mut module_orders = Vec::with_capacity(unit_order.len());

        for unit in unit_order {
            let module_order = self.load_unit_modules(sources, &unit_map, &mut module_map, unit)?;
            module_orders.push(module_order);
        }

        Ok(module_orders)
    }

    pub fn load_unit_modules(
        &mut self,
        sources: &mut Sources,
        unit_map: &Map<Unit>,
        map: &mut Map<Module>,
        unit: Unit,
    ) -> Result<Vec<Module>> {
        let unit_ent = &self.units[unit];
        let base_line = self.modules.len() as u32;

        let Ok(path) = unit_ent.get_absolute_source_path() else {
            return Err(error::Error::spanless(error::Kind::ModuleNotFound(
                Path::join(&unit_ent.root_path, &unit_ent.local_source_path)
            )));
        };

        let id = path.as_path().into();
        let module = self.modules.push(module::Ent::new(id));
        map.insert(id, module);

        let mut frontier = VecDeque::new();
        frontier.push_back((path, Span::default(), module));
        let mut ast_data = ast::Data::new();
        let mut ast_temp = ast::Temp::new();
        let mut graph = GenericGraph::new();
        let mut buffer = PathBuf::new();

        while let Some((path, span, slot)) = frontier.pop_front() {
            let content = std::fs::read_to_string(&path).map_err(|err| {
                error::Error::new(
                    error::Kind::ModuleLoadFailed(path.clone(), err), // todo: save module path
                    span,
                )
            })?;

            let source = SourceEnt::new(path, content);
            let source = sources.add(source);
            let content = sources.get(source).content();
            self.modules[slot].source = source;

            ast_data.clear();
            Parser::parse_imports(content, &mut ast_data, &mut ast_temp, source)
                .map_err(Convert::convert)?;

            if let Some(imports) = ModuleImports::new(&ast_data, &sources).imports() {
                for ModuleImport {
                    nick,
                    name,
                    path: path_span,
                } in imports
                {
                    buffer.clear();
                    let unit = unit_map
                        .get((sources.display(name), unit))
                        .copied()
                        .unwrap_or(unit);

                    buffer.push(&self.units[unit].root_path);
                    buffer.push(&self.units[unit].local_source_path);
                    buffer.set_extension("");
                    buffer.push(sources.display(path_span));
                    buffer.set_extension(Self::SOURCE_FILE_EXTENSION);

                    let Ok(path) = buffer.canonicalize() else {
                        return Err(error::Error::new(error::Kind::ModuleNotFound(buffer), path_span));
                    };

                    let id = path.as_path().into();

                    let id = if let Some(&id) = map.get(id) {
                        id
                    } else {
                        let module = self.modules.push(module::Ent::new(id));
                        map.insert(id, module);
                        frontier.push_back((path, path_span, module));
                        module
                    };

                    let name = nick.unwrap_or(name);
                    map.insert((sources.display(name), slot), id);
                    if id.0 >= base_line {
                        graph.add_edge(id.0 - base_line);
                    }
                }
            }
            graph.close_node();
        }

        let mut ordering = Vec::with_capacity(TreeStorage::<Module>::len(&graph));
        if let Some(mut cycle) = graph.detect_cycles(Module(0), Some(&mut ordering)) {
            cycle.iter_mut().for_each(|id| id.0 += base_line);
            return Err(Error::spanless(error::Kind::ModuleCycle(cycle)));
        }

        Ok(ordering)
    }

    pub fn load_units(
        &mut self,
        sources: &mut Sources,
        map: &mut Map<Unit>,
        root: &Path,
    ) -> Result<Vec<Unit>> {
        let mf_root = std::env::var(Self::RESOURCE_ROOT_VAR)
            .unwrap_or_else(|_| Self::DEFAULT_RESOURCE_ROOT_VAR.to_string());
        let mf_root = PathBuf::from(mf_root);

        let mut graph = GenericGraph::new();
        let mut buffer = PathBuf::new();

        let Ok(root) = root.canonicalize() else {
            return Err(Error::new(error::Kind::UnitNotFound(root.to_owned()), Span::default()));
        };

        let slot = self.units.push(unit::Ent::new());
        map.insert(root.as_path(), slot);

        let mut frontier = VecDeque::new();
        frontier.push_back((root, Span::default(), slot));
        let mut ast_data = ast::Data::new();
        let mut ast_temp = ast::Temp::new();

        while let Some((path, span, slot)) = frontier.pop_front() {
            buffer.clear();
            buffer.push(&path);
            buffer.push(Self::MANIFEST_LOCAL_PATH);
            let contents = std::fs::read_to_string(&buffer).map_err(|err| {
                Error::new(error::Kind::UnitLoadFailed(buffer.clone(), err), span)
            })?;

            // all parsing components assume source code is registered in sources
            let source = SourceEnt::new(buffer.clone(), contents);
            let source = sources.add(source);
            let contents = sources.get(source).content();

            self.units[slot].source = source;

            ast_data.clear();
            Parser::parse_manifest(contents, &mut ast_data, &mut ast_temp, source)
                .map_err(Convert::convert)?;

            let manifest = Manifest::new(&ast_data, &sources);

            if let Some(dependency) = manifest.dependencies() {
                for ManifestDepInfo {
                    name,
                    path: span_path,
                    version,
                } in dependency
                {
                    let path_str = sources.display(span_path);
                    let path = if path_str.starts_with(Self::GITHUB_DOMAIN) {
                        let path = Path::join(&mf_root, path_str);
                        if !path.exists() {
                            Self::download_git_repo(
                                &path,
                                path_str,
                                sources.display(version),
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

                    let id = if let Some(&id) = map.get(path.as_path()) {
                        id
                    } else {
                        let id = self.units.push(unit::Ent::new());
                        map.insert(path.as_path(), id);
                        frontier.push_back((path, span_path, id));
                        id
                    };

                    map.insert((sources.display(name), slot), id);
                    graph.add_edge(id.0);
                }
            }

            let root_path_str = manifest
                .get_string_tag("root")
                .unwrap_or(Self::DEFAULT_ROOT_SOURCE_PATH);
            // we preserve already loaded path segments
            self.units[slot].local_source_path = PathBuf::from(root_path_str);
            self.units[slot].root_path = path;

            graph.close_node();
        }

        let mut ordering = Vec::with_capacity(TreeStorage::<Unit>::len(&graph));
        if let Some(cycle) = graph.detect_cycles(slot, Some(&mut ordering)) {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_extension() {
        let mut path = PathBuf::from("/home/user/project/src/main.rs");
        path.set_extension("");

        assert_eq!(path.to_str(), Some("/home/user/project/src/main"));
    }

    #[test]
    fn test_no_cycle() {
        let mut map = Map::new();
        let mut sources = Sources::new();
        let mut modules = Modules::new();

        modules
            .load_units(&mut sources, &mut map, Path::new("src/tests/no_cycle"))
            .unwrap();

        assert_eq!(modules.units.len(), 3);
    }

    #[test]
    fn test_cycle() {
        let mut map = Map::new();
        let mut sources = Sources::new();
        let mut modules = Modules::new();

        assert!(matches!(
            modules
                .load_units(&mut sources, &mut map, Path::new("src/tests/cycle"))
                .unwrap_err()
                .kind(),
            error::Kind::UnitCycle(_)
        ));
    }

    #[test]
    fn full_load() {
        let mut sources = Sources::new();
        let mut modules = Modules::new();

        modules
            .load(&mut sources, Path::new("src/tests/no_cycle"))
            .unwrap();

        assert_eq!(modules.units.len(), 3);
        assert_eq!(modules.modules.len(), 6);
    }
}
