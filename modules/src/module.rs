use ast::*;
use lexer::*;
use module_types::{error::ModuleError, *};
use parser::*;
use storage::*;
use crate::state::*;

pub const SOURCE_FILE_EXTENSION: &'static str = "mf";
pub const MANIFEST_FILE_EXTENSION: &'static str = "mfm";
pub const RESOURCE_ROOT_VAR: &'static str = "MF_ROOT";
pub const DEFAULT_RESOURCE_ROOT_VAR: &'static str = ".mp_build_resources";
pub const MANIFEST_LOCAL_PATH: &'static str = "project.mfm";
pub const GITHUB_DOMAIN: &'static str = "github.com";
pub const DEFAULT_ROOT_SOURCE_PATH: &'static str = "src/root.mf";



impl ModuleBuilder<'_> {
    pub fn load_unit_modules(&mut self, unit: Unit) -> errors::Result<Vec<Source>> {
        self.loader_context.clear();
        let base_line = self.sources.len() as u32;

        {
            let unit_ent = &self.units[unit];
            let path = unit_ent.get_absolute_source_path().map_err(|trace| {
                self.diagnostics
                    .push(ModuleError::RootModuleNotFound { unit, trace });
            })?;

            let id = path.as_path().into();
            let module = self.sources.push(Default::default());
            self.modules[module] = module::ModuleEnt::new(id, path.as_path());
            self.module_map.insert(id, module);
            self.loader_context
                .module_frontier
                .push_back((path, Span::default(), module));
        }

        while let Some((path, span, slot)) = self.loader_context.module_frontier.pop_front() {
             {
                {
                    let Ok(content) = std::fs::read_to_string(&path).map_err(|err| {
                        self.diagnostics.push(ModuleError::ModuleLoadFail {
                            path: path.clone(),
                            trace: err,
                            loc: span,
                        });
                    }) else {
                        continue;
                    };

                    let source = SourceEnt::new(path, content);
                    self.sources[slot] = source;
                }

                self.loader_context.ast.clear();
                Parser::parse_imports(
                    self.sources,
                    self.diagnostics,
                    &mut self.loader_context.ast,
                    &mut self.loader_context.ast_temp,
                    slot,
                );
            }

            let mut counter = 0;
            if let Some(imports) = ModuleImports::new(&self.loader_context.ast, &self.sources).imports() {
                for ModuleImport {
                    nick,
                    name,
                    path: path_span,
                } in imports
                {
                    {
                        let unit = self
                            .loader_context
                            .map
                            .get((self.sources.display(name), unit))
                            .copied()
                            .unwrap_or(unit);

                        self.loader_context.buffer.clear();
                        self.loader_context.buffer.push(&self.units[unit].root_path);
                        self.loader_context.buffer.push(&self.units[unit].local_source_path);
                        self.loader_context.buffer.set_extension("");
                        self.loader_context.buffer.push(self.sources.display(path_span));
                        self.loader_context.buffer.set_extension(SOURCE_FILE_EXTENSION);
                    }

                    let id = {
                        let Ok(path) = self.loader_context.buffer.canonicalize().map_err(|err| {
                            self.diagnostics.push(ModuleError::ModuleNotFound {
                                trace: err,
                                path: self.loader_context.buffer.clone(),
                                loc: path_span,
                            });
                        }) else {
                            continue;
                        };

                        let id = path.as_path().into();

                        if let Some(&id) = self.module_map.get(id) {
                            id
                        } else {
                            let module = Source::new(self.sources.len() + counter);
                            counter += 1;
                            self.modules[module] = module::ModuleEnt::new(id, path.as_path());
                            self.module_map.insert(id, module);
                            self.loader_context
                                .module_frontier
                                .push_back((path, path_span, module));
                            module
                        }
                    };

                    self.modules[slot].dependency.push(id);

                    self.module_map
                        .insert((self.sources.display(nick), slot), id);
                    if id.0 >= base_line {
                        self.loader_context.cycle_graph.add_edge(id.0 - base_line);
                    }
                }
            }
            for _ in 0..counter {
                self.sources.push(Default::default());
            }

            self.loader_context.cycle_graph.close_node(0);
        }

        let mut ordering =
            Vec::with_capacity(TreeStorage::<Source>::max_node(&self.loader_context.cycle_graph));
        self.loader_context
            .cycle_graph
            .detect_cycles(Source(0), Some(&mut ordering))
            .map_err(|mut err| {
                err.iter_mut().for_each(|id| id.0 += base_line);
                self.diagnostics
                    .push(ModuleError::ModuleCycle { cycle: err });
            })?;

        ordering.iter_mut().for_each(|id| id.0 += base_line);

        Ok(ordering)
    }
}

pub struct ModuleImports<'a> {
    ast_data: &'a AstData,
    sources: &'a Sources,
}

impl<'a> ModuleImports<'a> {
    pub fn new(ast_data: &'a AstData, sources: &'a Sources) -> Self {
        ModuleImports { ast_data, sources }
    }

    pub fn imports(&self) -> Option<impl Iterator<Item = ModuleImport> + 'a> {
        self.ast_data.elements().next().map(|(_, e)| {
            assert!(e.kind != AstKind::Import);
            self.ast_data.conns.get(e.children).iter().map(|&c| {
                let &[nick, path] = self.ast_data.children(c) else {
                    unreachable!();
                };

                let path = self.ast_data.nodes[path].span.strip_sides();
                let nick = if nick.is_reserved_value() {
                    let split_index = self
                        .sources
                        .display(path)
                        .rfind('/')
                        .map(|i| i + 1)
                        .unwrap_or(0);
                    path.slice(split_index..)
                } else {
                    self.ast_data.nodes[nick].span
                };

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
    pub nick: Span,
    pub name: Span,
    pub path: Span,
}
