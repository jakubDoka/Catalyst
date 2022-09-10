use std::{
    default::default,
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

type DiagLoc = Option<(Ident, Span)>;
type PackageFrontier = BumpVec<(Ident, PathBuf, DiagLoc)>;
type ModuleFrontier = BumpVec<(Ident, Ident, PathBuf, DiagLoc)>;

impl PackageLoader<'_> {
    /// Loads the project into graph of manifests and source files.
    pub fn load(&mut self, root: &Path) {
        drop(self.load_low(root))
    }

    fn load_low(&mut self, root: &Path) -> errors::Result {
        // setup
        let mut path = root
            .canonicalize()
            .map_err(|err| self.file_error(None, root, "manifest is missing", err))?;
        path.push("package");
        path.set_extension(MANIFEST_EXTENSION);
        let id = self.intern_path(&path)?;
        path.pop();
        let mut frontier = bumpvec![(id, path, default())];
        let mut ast_data = AstData::new();
        let mut parser_state = ParserState::new();

        // load packages
        while !self.load_package(&mut ast_data, &mut parser_state, &mut frontier, root)? {}

        // load modules
        let Some(&ModKind::Package { ref root_module, span }) = self.packages.modules.get(&id).map(|m| &m.kind) else {
            unreachable!();
        };
        let path = root_module.to_owned();
        let module_id = self.load_modules(&path, id, span)?;

        // prepare cycle detection, we are doing packages and modules in one go
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

        // detect cycles and build ordering
        let roots = [id.index() as u32, module_id.index() as u32];
        let mut ordering = bumpvec![cap self.packages.modules.len()];
        self.package_graph
            .ordering(roots, &mut ordering)
            .map_err(|cycle| self.dependency_cycle(cycle))?;

        // `SAFETY`: We previously supplied Ident internals into the graph
        // so it is safe to construct them from what graph gave us
        self.packages.module_order.extend(
            ordering
                .into_iter()
                .map(|i| unsafe { Ident::new(i as usize) })
                .filter(|id| self.packages.modules.get(&id).unwrap().is_module()),
        );

        Ok(())
    }

    /// Loads one package and pushes its dependencies to frontier.
    /// Downloads packages if needed.
    fn load_package(
        &mut self,
        ast_data: &mut AstData,
        parser_state: &mut ParserState,
        frontier: &mut PackageFrontier,
        project_path: &Path,
    ) -> errors::Result<bool> {
        let Some((id, mut tmp_path, loc)) = frontier.pop() else {
            return Ok(true);
        };

        // load content
        tmp_path.push("package");
        tmp_path.set_extension(MANIFEST_EXTENSION);
        let content = read_to_string(&tmp_path)
            .map_err(|err| self.file_error(loc, &tmp_path, "cannot load manifest", err))?;
        let path = tmp_path.clone();
        tmp_path.pop();

        // parse
        ast_data.clear();
        parser_state.start(&content, id, false);
        let fields = Parser::new(&content, parser_state, ast_data, self.workspace).parse_manifest();

        // retrieve data from ast
        let (mut root_module, span) = {
            let (root_module, span) = self
                .find_field_value("root", fields, &content, ast_data)
                .unwrap_or(("root", default()));
            (tmp_path.join(root_module), span)
        };
        root_module.set_extension("");
        let deps =
            self.find_field("deps", fields, &content, ast_data)
                .map_or(default(), |deps_ast| {
                    self.push_package_deps(
                        id,
                        &mut tmp_path,
                        deps_ast,
                        &content,
                        ast_data,
                        frontier,
                        project_path,
                    )
                });

        // save
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

    /// Function will find and optionally download all direct dependencies.
    fn push_package_deps(
        &mut self,
        id: Ident,
        temp_path: &mut PathBuf,
        deps_ast: Ast,
        content: &str,
        ast_data: &AstData,
        frontier: &mut PackageFrontier,
        project_path: &Path,
    ) -> VSlice<Dep> {
        self.packages.conns.start_cache();
        for dep in &ast_data[ast_data[deps_ast.children][1].children] {
            // retrieve data from ast
            let &Ast { kind: AstKind::ManifestImport { use_git }, children, .. } = dep else {
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
            let version_loc = version_span.map(|span| (id, span));
            let current_loc = Some((id, path_span));

            // find the path of the dependency
            let path_result = if use_git {
                self.download_package(project_path, current_loc, version_loc, version, path_str)
            } else {
                let to_pop = Path::new(path_str).components().count();
                temp_path.push(path_str);
                let path = temp_path.canonicalize().map_err(|err| {
                    self.file_error(
                        current_loc,
                        temp_path,
                        "cannot canonicalize local package path",
                        err,
                    )
                });
                assert!((0..to_pop).fold(true, |acc, _| acc & temp_path.pop()));
                path
            };

            let Ok(path) = path_result else {
                continue;
            };

            let Ok(ptr) = self.intern_path(path.as_path()) else {
                continue;
            };

            // save
            let dep = Dep { name, ptr };
            self.packages.conns.cache(dep);

            if self.packages.modules.get(&ptr).is_none() {
                frontier.push((ptr, path, current_loc));
            }
        }

        self.packages.conns.bump_cached()
    }

    /// Only reachable modules are loaded.
    fn load_modules(
        &mut self,
        path: &Path,
        package: Ident,
        loc: Maybe<Span>,
    ) -> errors::Result<Ident> {
        let loc = loc.expand().map(|loc| (package, loc));

        let path = path
            .with_extension(FILE_EXTENSION)
            .canonicalize()
            .map_err(|err| {
                self.file_error(loc, path, "cannot canonicalize root module path", err)
            })?;

        let id = self.intern_path(&path)?;

        let mut frontier = bumpvec![(id, package, path, default())];
        let mut ast_data = AstData::new();
        let mut parser_state = ParserState::new();

        while !self.load_module(&mut ast_data, &mut parser_state, &mut frontier)? {}

        Ok(id)
    }

    /// Returns true when last module was already loaded.
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
        parser_state.start(&content, id, false);
        let imports =
            Parser::new(&content, parser_state, ast_data, &mut self.workspace).parse_imports();

        let deps = if let Some(imports) = imports {
            self.load_module_deps(id, package_id, imports, &content, ast_data, frontier)
                .unwrap_or(default())
        } else {
            default()
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
        imports: Ast,
        content: &str,
        ast_data: &AstData,
        frontier: &mut ModuleFrontier,
    ) -> errors::Result<VSlice<Dep>> {
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
            // $package$(/$module_path)?
            let (package, module_path) = path_str.split_once('/').unwrap_or((path_str, ""));

            // Try to find package mentioned by import, '.' is a self reference.
            let package_ent = &self.packages.modules.get(&package_id).unwrap();
            let maybe_package = self.packages.conns[package_ent.deps]
                .iter()
                .find_map(|dep| {
                    (&package_ent.content[dep.name.range()] == package).then_some(dep.ptr)
                })
                .or_else(|| (package == ".").then_some(package_id));
            let Some(external_package_id) = maybe_package else {
                let sippet = Self::unknown_package(self, path_span.sliced(..package.len()), id, &package_ent);
                self.workspace.push(sippet);
                continue;
            };

            // All submodules of a package are expected to be in subdirectory with same name and level as
            // root module.
            let external_package = &self.packages.modules.get(&external_package_id).unwrap();
            let ModKind::Package { ref root_module, .. } = external_package.kind else {
                unreachable!();
            };
            let mut path = root_module.with_extension("").join(module_path);
            path.set_extension(FILE_EXTENSION);

            let import_loc = Some((id, path_span));

            let path = match path.canonicalize() {
                Ok(path) => path,
                Err(err) => {
                    self.file_error(import_loc, &path, "cannot canonicalize module path", err);
                    continue;
                }
            };

            let import_id = self.intern_path(&path)?;

            let dep = Dep {
                name: name.span,
                ptr: import_id,
            };
            self.packages.conns.cache(dep);

            if self.packages.modules.contains_key(&import_id) {
                continue;
            }

            frontier.push((import_id, external_package_id, path, import_loc));
        }

        Ok(self.packages.conns.bump_cached())
    }

    /// git is invoked and package may be downloaded into `%CATALYST_CACHE%/url/(version || 'main')`
    fn download_package(
        &mut self,
        project_path: &Path,
        loc: DiagLoc,
        version_loc: DiagLoc,
        version: Option<&str>,
        url: &str,
    ) -> errors::Result<PathBuf> {
        let full_url = &format!("https://{}", url);
        let rev_owned = self.resolve_version(version_loc, version, &full_url)?;
        if version.is_some() && rev_owned.is_none() {
            self.invalid_version(version_loc);
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
        loc: DiagLoc,
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

    fn execute_git(
        &mut self,
        loc: DiagLoc,
        quiet: bool,
        args: impl IntoIterator<Item = &str> + Clone,
    ) -> errors::Result<String> {
        if !quiet {
            self.git_info(loc, args.clone());
        }

        let output = Command::new("git").args(args.clone()).output();
        let output = output.map_err(|err| self.git_exec_error(loc, args.clone(), err))?;

        if !output.status.success() {
            let output = String::from_utf8_lossy(&output.stderr);
            self.git_exit_error(loc, args, output.as_ref());
            return Err(());
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    fn intern_path(&mut self, path: &Path) -> errors::Result<Ident> {
        let Some(str_path) = path.to_str() else {
            self.invalid_path_encoding(default(), path, "manifest");
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
        fields: VSlice<Ast>,
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
        fields: VSlice<Ast>,
        content: &str,
        ast_data: &AstData,
    ) -> Option<Ast> {
        ast_data[fields]
            .iter()
            .find(|field| &content[ast_data[field.children][0].span.range()] == name)
            .copied()
    }

    gen_error_fns! {
        push file_error(self, loc: DiagLoc, path: &Path, message: &str, err: std::io::Error) {
            err: ("{}", message);
            info: ("related path: {}", path.display());
            info: ("trace: {}", err);
            (loc?.1, loc?.0) {
                err[loc?.1]: "caused by this";
            }
        }

        push invalid_path_encoding(self, loc: DiagLoc, path: &Path, kind: &str) {
            err: ("invalid {} path encoding", kind);
            info: ("related path: {}", path.display());
            (loc?.1, loc?.0) {
                err[loc?.1]: "caused by this";
            }
        }

        push dependency_cycle(self, cycle: Vec<u32>) {
            err: "dependency cycle detected";
            info: (
                "cycle:\n{}",
                cycle
                    .iter()
                    .map(|&id| &self.interner[unsafe { Ident::new(id as usize) }])
                    .collect::<BumpVec<_>>()
                    .join("\n"),
            );
        }

        print git_info(self, loc: DiagLoc, args: impl IntoIterator<Item = &str>) {
            info: ("executing git: {}", args.into_iter().collect::<BumpVec<_>>().join(" "));
            (loc?.1, loc?.0) {
                info[loc?.1]: "invocation declared here";
            }
        }

        push git_exec_error(self, loc: DiagLoc, args: impl IntoIterator<Item = &str>, err: std::io::Error) {
            err: "git execution failed";
            info: ("executing git: {}", args.into_iter().collect::<BumpVec<_>>().join(" "));
            info: ("trace: {}", err);
            (loc?.1, loc?.0) {
                info[loc?.1]: "invocation declared here";
            }
        }

        push git_exit_error(self, loc: DiagLoc, args: impl IntoIterator<Item = &str>, output: &str) {
            err: "git execution failed";
            info: ("executing git: {}", args.into_iter().collect::<BumpVec<_>>().join(" "));
            info: ("output:\n{}", output);
            (loc?.1, loc?.0) {
                info[loc?.1]: "invocation declared here";
            }
        }

        push invalid_version(self, loc: DiagLoc) {
            warn: "invalid version";
            (loc?.1, loc?.0) {
                info[loc?.1]: "version is specified here";
            }
        }

        unknown_package(s: &Self, span: Span, id: Ident, package: &Mod) {
            err: "cannot find this package in manifest";
            info: (
                "available packages: '{}'",
                s.packages.conns[package.deps]
                    .iter()
                    .map(|dep| &package.content[dep.name.range()])
                    .chain(std::iter::once("."))
                    .collect::<BumpVec<_>>()
                    .join("', '")
            );
            (s.packages.reveal_span_lines(id, span), id) {
                err[span]: "invalid package prefix used here";
            }
        }
    }
}
