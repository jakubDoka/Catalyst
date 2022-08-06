use std::{
    fs::{create_dir_all, read_to_string},
    path::*,
    process::Command,
};

use crate::*;
use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;

pub const MANIFEST_EXTENSION: &str = "ctlm";
pub const FILE_EXTENSION: &str = "ctl";
pub const DEP_ROOT_VAR: &str = "CATALYST_DEP_ROOT";
pub const DEFAULT_DEP_ROOT: &str = "deps";

type PackageFrontier = Vec<(Ident, PathBuf, Maybe<Loc>)>;
type ModuleFrontier = Vec<(Ident, Ident, PathBuf, Maybe<Loc>)>;

impl PackageLoader<'_> {
    pub fn load(&mut self, root: &Path) -> errors::Result {
        let mut path = root.to_owned();
        path.push("package");
        path.set_extension(MANIFEST_EXTENSION);
        let id = self.intern_path(&path)?;
        path.pop();

        let mut frontier = vec![(id, path, Maybe::none())];
        let mut ast_data = AstData::new();
        let mut parser_state = ParserState::new();

        loop {
            if let Ok(true) =
                self.load_package(&mut ast_data, &mut parser_state, &mut frontier, root)
            {
                break;
            }
        }

        if self.workspace.has_errors() {
            return Err(());
        }

        let Some(&ModKind::Package { ref root_module, span }) = self.packages.modules.get(id).map(|m| &m.kind) else {
            unreachable!();
        };

        let path = root_module.to_owned();
        let module_id = self.load_modules(&path, id, span)?;

        self.package_graph.clear();
        self.package_graph
            .load_nodes(self.packages.modules.keys().map(|id| id.index() as u32));

        for (k, module) in self.packages.modules.iter() {
            self.package_graph.new_node(k.index() as u32).add_edges(
                self.packages.conns[module.deps]
                    .iter()
                    .map(|dep| dep.ptr.index() as u32),
            );
        }

        let roots = [id.index() as u32, module_id.index() as u32];
        let mut ordering = Vec::with_capacity(self.packages.modules.len());
        self.package_graph
            .ordering(roots, &mut ordering)
            .map_err(|cycle| self.dependency_cycle(cycle))?;

        self.packages
            .module_order
            .extend(ordering.into_iter().map(|i| Ident::new(i as usize)));

        Ok(())
    }

    fn load_package(
        &mut self,
        ast_data: &mut AstData,
        parser_state: &mut ParserState,
        frontier: &mut PackageFrontier,
        project_path: &Path,
    ) -> errors::Result<bool> {
        let Some((id, mut path, loc)) = frontier.pop() else {
            return Ok(true);
        };

        path.push("package");
        path.set_extension(MANIFEST_EXTENSION);

        let content = read_to_string(&path)
            .map_err(|err| self.file_error(loc, &path, "cannot load manifest", err))?;

        path.pop();

        ast_data.clear();
        parser_state.start(&content, id);
        let fields = Parser::new(&content, parser_state, ast_data, self.workspace).parse_manifest();

        let (mut root_module, span) = {
            let (root_module, span) = self
                .find_field_value("root", fields, &content, ast_data)
                .unwrap_or(("root", Maybe::none()));
            (path.join(root_module), span)
        };
        root_module.set_extension("");

        let deps =
            self.find_field("deps", fields, &content, ast_data)
                .map_or(Maybe::none(), |deps_ast| {
                    self.load_package_deps(
                        id,
                        &mut path,
                        deps_ast,
                        &content,
                        ast_data,
                        frontier,
                        project_path,
                    )
                });

        let package = Mod {
            path,
            line_mapping: LineMapping::new(&content),
            content,
            kind: ModKind::Package { root_module, span },
            deps,
        };
        self.packages.modules.insert(id, package);

        Ok(false)
    }

    fn load_package_deps(
        &mut self,
        id: Ident,
        temp_path: &mut PathBuf,
        deps_ast: AstEnt,
        content: &str,
        ast_data: &AstData,
        frontier: &mut PackageFrontier,
        project_path: &Path,
    ) -> Maybe<DepList> {
        self.packages.conns.start_cache();
        for dep in &ast_data[ast_data[deps_ast.children][1].children] {
            let &AstEnt { kind: AstKind::ManifestImport { use_git }, children, .. } = dep else {
                unreachable!("{:?}", dep.kind);
            };

            let [name, path, version] = &ast_data[children] else {
                unreachable!();
            };

            let path_span = path.span.shrink(1);
            let path_str = &content[path_span.range()];

            let name = if name.kind.is_none() {
                let start = path_str.rfind("/").map(|i| i + 1).unwrap_or(0);
                path_span.sliced(start..)
            } else {
                name.span
            };

            let version_span = (version.kind != AstKind::None).then(|| version.span.shrink(1));
            let version = version_span.map(|span| &content[span.range()]);

            let version_loc = Loc {
                source: id,
                span: version_span.into(),
            }
            .into();

            let current_loc = Loc {
                source: id,
                span: path_span.into(),
            }
            .into();

            let path_result = if use_git {
                self.download_package(project_path, current_loc, version_loc, version, path_str)
            } else {
                let to_pop = Path::new(path_str).components().count();
                temp_path.push(path_str);
                let p = temp_path.canonicalize().map_err(|err| {
                    self.file_error(
                        current_loc,
                        temp_path,
                        "cannot canonicalize local package path",
                        err,
                    )
                });
                assert!((0..to_pop).fold(true, |acc, _| acc & temp_path.pop()));
                p
            };

            let Ok(path) = path_result else {
                continue;
            };

            let Ok(ptr) = self.intern_path(path.as_path()) else {
                continue;
            };

            let dep = Dep { name, ptr };
            self.packages.conns.cache(dep);

            if self.packages.modules.get(ptr).is_none() {
                frontier.push((ptr, path, current_loc));
            }
        }

        self.packages.conns.bump_cached()
    }

    fn load_modules(
        &mut self,
        path: &Path,
        package: Ident,
        loc: Maybe<Span>,
    ) -> errors::Result<Ident> {
        let id = self.intern_path(path)?;

        let loc = Loc {
            source: package,
            span: loc,
        }
        .into();

        let path = path
            .with_extension(FILE_EXTENSION)
            .canonicalize()
            .map_err(|err| {
                self.file_error(loc, path, "cannot canonicalize root module path", err)
            })?;

        let mut frontier = vec![(id, package, path, Maybe::none())];
        let mut ast_data = AstData::new();
        let mut parser_state = ParserState::new();

        loop {
            if let Ok(true) = self.load_module(&mut ast_data, &mut parser_state, &mut frontier) {
                break;
            }
        }

        Ok(id)
    }

    fn load_module(
        &mut self,
        ast_data: &mut AstData,
        parser_state: &mut ParserState,
        frontier: &mut ModuleFrontier,
    ) -> errors::Result<bool> {
        let Some((id, package_id, path, loc)) = frontier.pop() else {
            return Ok(true);
        };

        let content = read_to_string(&path)
            .map_err(|err| self.file_error(loc, &path, "cannot load source file", err))?;

        ast_data.clear();
        parser_state.start(&content, id);
        let imports =
            Parser::new(&content, parser_state, ast_data, &mut self.workspace).parse_imports();

        let deps = if let Some(imports) = imports {
            self.load_module_deps(id, package_id, imports, &content, ast_data, frontier)
                .unwrap_or(Maybe::none())
        } else {
            Maybe::none()
        };

        let module = Mod {
            path,
            line_mapping: LineMapping::new(&content),
            content,
            kind: ModKind::Module {
                package: package_id,
                ordering: 0,
                items: vec![],
            },
            deps,
        };
        self.packages.modules.insert(id, module);

        Ok(false)
    }

    fn load_module_deps(
        &mut self,
        id: Ident,
        package_id: Ident,
        imports: AstEnt,
        content: &str,
        ast_data: &AstData,
        frontier: &mut ModuleFrontier,
    ) -> errors::Result<Maybe<DepList>> {
        self.packages.conns.start_cache();

        for import in &ast_data[imports.children] {
            let [mut name, path] = &ast_data[import.children] else {
                unreachable!();
            };

            // name defaults to last segment of module path
            if name.kind.is_none() {
                let span = path.span.shrink(1);
                let start = content[span.range()].rfind('/').map(|i| i + 1).unwrap_or(0);
                name.span = span.sliced(start..);
            }

            let path_span = path.span.shrink(1);
            let path_str = &content[path_span.range()];
            let (package, module_name) = path_str.split_once('/').unwrap_or((path_str, ""));

            let package_ent = &self.packages.modules.get(package_id).unwrap();
            let maybe_package = self.packages.conns[package_ent.deps]
                .iter()
                .find_map(|dep| {
                    (&package_ent.content[dep.name.range()] == package).then_some(dep.ptr)
                })
                .or_else(|| (package == ".").then_some(package_id));

            let Some(external_package_id) = maybe_package else {
                self.workspace.push(diag! {
                    (path_span.sliced(..package.len()), id)
                    error => "cannot find this package in manifest",
                    (none) => "available packages: '{}'" { 
                        self.packages.conns[package_ent.deps]
                            .iter()
                            .map(|dep| &package_ent.content[dep.name.range()])
                            .chain(std::iter::once("."))
                            .collect::<Vec<_>>()
                            .join("', '")
                    },
                });
                continue;
            };

            let external_package = &self.packages.modules.get(external_package_id).unwrap();
            let ModKind::Package { ref root_module, .. } = external_package.kind else {
                unreachable!();
            };
            let mut path = root_module.with_extension("").join(module_name);
            path.set_extension(FILE_EXTENSION);

            let import_loc = Loc {
                source: id,
                span: path_span.into(),
            }
            .into();

            path = path.canonicalize().map_err(|err| {
                self.file_error(import_loc, &path, "cannot canonicalize module path", err)
            })?;

            let import_id = self.intern_path(&path)?;

            let dep = Dep {
                name: name.span,
                ptr: import_id,
            };
            self.packages.conns.cache(dep);

            if self.packages.modules.get(import_id).is_none() {
                frontier.push((import_id, external_package_id, path, import_loc));
            }
        }

        Ok(self.packages.conns.bump_cached())
    }

    fn download_package(
        &mut self,
        project_path: &Path,
        loc: Maybe<Loc>,
        version_loc: Maybe<Loc>,
        version: Option<&str>,
        url: &str,
    ) -> errors::Result<PathBuf> {
        let full_url = &format!("https://{}", url);
        let rev_owned = self.resolve_version(version_loc, version, &full_url)?;
        if version.is_some() && rev_owned.is_none() {
            self.workspace.push(diag! {
                (exp version_loc) warning => "cannot find this version, using main branch instead",
            })
        }
        let rev = rev_owned.as_ref().map(|v| v.as_str());

        let mut dep_root = self.get_dep_root(project_path);
        dep_root.push(url);
        dep_root.push(
            rev.map(|rev| &rev[..rev.find('.').unwrap_or(rev.len())])
                .unwrap_or("main"),
        );

        let exists = dep_root.exists();

        create_dir_all(&dep_root)
            .map_err(|err| self.file_error(loc, &dep_root, "cannot create directory", err))?;

        let install_path = dep_root.canonicalize().map_err(|err| {
            self.file_error(loc, &dep_root, "cannot canonicalize installation path", err)
        })?;

        if exists {
            return Ok(install_path);
        }

        let id = self.intern_path(install_path.as_path())?;

        let fixed_args = [
            "clone",
            "--depth",
            "1",
            "--filter",
            "blob:none",
            &full_url,
            &self.interner[id].to_owned(),
        ];
        let optional_args = rev.map(|rev| ["--branch", rev]);
        let args = fixed_args
            .into_iter()
            .chain(optional_args.into_iter().flatten());

        self.execute_git(loc, false, args)?;

        Ok(install_path)
    }

    fn resolve_version(
        &mut self,
        loc: Maybe<Loc>,
        version: Option<&str>,
        url: &str,
    ) -> errors::Result<Option<String>> {
        let Some(version) = version else {
            return Ok(None);
        };

        let args = ["ls-remote", url, &format!("refs/tags/{}", version)];

        let output = self.execute_git(loc, true, args)?;

        Ok(output
            .lines()
            .filter_map(|line| line.split_whitespace().nth(1))
            .filter_map(|path| path.strip_prefix("refs/tags/"))
            .map(|version| {
                version[1..]
                    .split('.')
                    .take(3)
                    .filter_map(|component| component.parse::<u32>().ok())
            })
            .filter_map(|mut comps| Some((comps.next()?, comps.next()?, comps.next()?)))
            .max()
            .map(|(major, minor, patch)| format!("v{}.{}.{}", major, minor, patch)))
    }

    fn execute_git<'a>(
        &mut self,
        loc: Maybe<Loc>,
        quiet: bool,
        args: impl IntoIterator<Item = &'a str> + Clone,
    ) -> errors::Result<String> {
        if !quiet {
            self.workspace.push(diag! {
                (exp loc) hint => "executing: git {}" {
                    args.clone().into_iter().collect::<Vec<_>>().join(" ")
                }
            });
        }

        let output = Command::new("git").args(args).output();
        let output = output.map_err(|err| {
            self.workspace.push(diag! {
                (exp loc) error => "cannot execute git",
                (none) => "error: {}" { err }
            })
        })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            self.workspace
                .push(diag!((exp loc) error => "git error:\n{}" { stderr }));
            return Err(());
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    fn intern_path(&mut self, path: &Path) -> errors::Result<Ident> {
        let Some(str_path) = path.to_str() else {
            self.invalid_path_encoding(Maybe::none(), path, "manifest");
            return Err(());
        };
        Ok(self.interner.intern_str(str_path))
    }

    fn get_dep_root(&mut self, project_path: &Path) -> PathBuf {
        let var = std::env::var(DEP_ROOT_VAR).unwrap_or(DEFAULT_DEP_ROOT.into());
        project_path.join(var)
    }

    fn find_field_value<'a>(
        &self,
        name: &str,
        fields: Maybe<AstList>,
        content: &'a str,
        ast_data: &AstData,
    ) -> Option<(&'a str, Maybe<Span>)> {
        self.find_field(name, fields, content, ast_data)
            .map(|ast| ast_data[ast.children][1].span.shrink(1))
            .map(|span| (&content[span.range()], Maybe::some(span)))
    }

    fn find_field(
        &self,
        name: &str,
        fields: Maybe<AstList>,
        content: &str,
        ast_data: &AstData,
    ) -> Option<AstEnt> {
        ast_data[fields]
            .iter()
            .find(|field| &content[ast_data[field.children][0].span.range()] == name)
            .copied()
    }

    fn file_error(&mut self, loc: Maybe<Loc>, path: &Path, message: &str, err: std::io::Error) {
        self.workspace.push(diag! {
            (exp loc) error => "{}" { message },
            (none) => "related path: {}" { path.display() },
            (none) => "trace: {}" { err },
        });
    }

    fn invalid_path_encoding(&mut self, loc: Maybe<Loc>, path: &Path, kind: &str) {
        self.workspace.push(diag! {
            (exp loc) error => "invalid {} path encoding" { kind },
            (none) => "path: {}" { path.display() },
        });
    }

    fn dependency_cycle(&mut self, cycle: Vec<u32>) {
        self.workspace.push(diag! {
            (none) error => "dependency cycle detected",
            (none) => "cycle:\n\t{}" {
                cycle.into_iter()
                    .map(|id| &self.interner[Ident::new(id as usize)])
                    .collect::<Vec<_>>()
                    .join("\n\t")
            },
        });
    }
}
