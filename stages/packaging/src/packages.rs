use std::{
    borrow::Cow,
    default::default,
    env::{self, VarError},
    ffi::{OsStr, OsString},
    io, iter,
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

#[derive(Default)]
pub struct Context {
    packages: Map<PathBuf, DummyPackage>,
    package_frontier: Vec<PathBuf>,
    parsing_state: ParsingState,
    ast_data: AstData,
    dep_root: PathBuf,
}

pub struct DummyPackage {
    pub root_module: PathBuf,
    pub deps: Vec<(NameAst, PathBuf)>,
    pub source: VRef<Source>,
}

impl PackageLoader<'_> {
    /// Loads the project into graph of manifests and source files.
    pub fn load(&mut self, root_path: &Path, ctx: &mut Context) -> Option<()> {
        ctx.dep_root = self.resolve_dep_root_path(root_path)?;
        let root_path = self.resolve_root_path(root_path)?;
        let root_package = self.load_packages(root_path, ctx)?;

        todo!()
    }

    fn resolve_dep_root_path(&mut self, root_path: &Path) -> Option<PathBuf> {
        let path = match self.resources.db.var("CATALYST_DEP_ROOT") {
            Ok(path) if Path::new(&path).is_absolute() => path.into(),
            Ok(path) => root_path.join(path),
            Err(VarError::NotPresent) => {
                let default_path = root_path.join("deps");
                self.resources
                    .db
                    .create_dir_all(&default_path)
                    .map_err(|err| self.cannot_create_dep_root(err, &default_path))
                    .ok()?;
                default_path
            }
            Err(err) => self.invalid_dep_root_encoding(err)?,
        };

        let root = self
            .resources
            .db
            .canonicalize(&path)
            .map_err(|err| self.invalid_dep_root(err, &path))
            .ok()?;

        Some(root)
    }

    fn load_packages(&mut self, root_path: PathBuf, ctx: &mut Context) -> Option<VRef<Package>> {
        ctx.package_frontier.push(root_path);
        while let Some(path) = ctx.package_frontier.pop() {
            self.load_package(path, ctx);
        }

        todo!()
    }

    fn load_package(&mut self, path: PathBuf, ctx: &mut Context) -> Option<()> {
        let package_path = path.join("package").with_extension("ctlm");

        let source_id = self.load_source(package_path)?;
        let manifest = self.parse_manifest(source_id, ctx)?;
        let root_module_path = self.resolve_root_module_path(&path, source_id, manifest)?;
        let deps = self.resolve_manifest_deps(&path, source_id, manifest, ctx);

        Some(())
    }

    fn resolve_manifest_deps(
        &mut self,
        path: &Path,
        source_id: VRef<Source>,
        manifest: ManifestAst,
        ctx: &Context,
    ) -> Vec<(NameAst, PathBuf)> {
        let mut deps = Vec::with_capacity(manifest.deps.len());
        for &ManifestDepAst {
            git,
            name,
            path,
            version,
            ..
        } in manifest.deps.iter()
        {
            let path = if git {
            } else {
            };
        }

        deps
    }

    fn resolve_root_module_path(
        &mut self,
        root_path: &Path,
        source_id: VRef<Source>,
        manifest: ManifestAst,
    ) -> Option<PathBuf> {
        let field_name = self.interner.intern_str("root");
        let value_span = manifest
            .find_field(field_name)
            .and_then(|root| match root.value {
                ManifestValueAst::String(str) => Some(str),
                _ => self.invalid_manifest_root_field(root, source_id)?,
            });

        let content = &self.resources.sources[source_id].content;

        let name = value_span.map_or("root", |str| &content[str.shrink(1).range()]);
        let full_path = root_path.join(name).with_extension("ctl");
        self.resolve_module_path(&full_path)
    }

    fn parse_manifest<'a>(
        &mut self,
        source_id: VRef<Source>,
        ctx: &'a mut Context,
    ) -> Option<ManifestAst<'a>> {
        let source = &self.resources.sources[source_id];
        ctx.parsing_state.start(&source.content);
        ctx.ast_data.clear();

        let manifest = ParsingCtx::new(
            &source.content,
            &mut ctx.parsing_state,
            &ctx.ast_data,
            self.workspace,
            self.interner,
            source_id,
        )
        .parse::<ManifestAst>();

        manifest
    }

    fn load_source(&mut self, path: PathBuf) -> Option<VRef<Source>> {
        let content = self
            .resources
            .db
            .read_to_string(&path)
            .map_err(|err| self.unreachable_source(&path, err))
            .ok()?;

        let source = Source {
            path,
            line_mapping: LineMapping::new(&content),
            content,
        };

        Some(self.resources.sources.push(source))
    }

    fn resolve_root_path(&mut self, root: &Path) -> Option<PathBuf> {
        self.resources
            .db
            .canonicalize(root)
            .map_err(|err| self.invalid_package_root(root, err))
            .and_then(|root| match root.is_dir() {
                true => Ok(root),
                false => Err(self.non_dir_package_root(root)),
            })
            .ok()
    }

    fn resolve_module_path(&mut self, path: &Path) -> Option<PathBuf> {
        self.resources
            .db
            .canonicalize(path)
            .map_err(|err| self.invalid_module_path(path, err))
            .and_then(|path| match path.is_file() {
                true => Ok(path),
                false => Err(self.non_file_module_path(path)),
            })
            .ok()
    }

    /// git is invoked and package may be downloaded into `%CATALYST_CACHE%/url/(version || 'main')`
    fn download_package(
        &mut self,
        source: VRef<Source>,
        span: Span,
        version: Option<Span>,
        url: &str,
        ctx: &Context,
    ) -> Option<PathBuf> {
        let full_url = &format!("https://{}", url);
        let versions = self.resolve_version(source, version, full_url)?;
        if let Some(version) = version && versions.is_empty() {
            self.invalid_version(source, version);
        }

        let max_version = versions
            .iter()
            .max()
            .map(|(major, minor, patch)| format!("{}.{}.{}", major, minor, patch))
            .unwrap_or("main".to_string());

        let download_root = ctx.dep_root.join(url).join(max_version);

        let exists = download_root.exists();

        self.resources
            .db
            .create_dir_all(&download_root)
            .map_err(|err| self.cannot_create_download_dir(err, &download_root))
            .ok()?;

        let install_path = self
            .resources
            .db
            .canonicalize(&download_root)
            .expect("since we just created it there is no reason for this to fail");

        if exists {
            return Some(install_path);
        }

        let str_args = [
            "clone",
            "--depth",
            "1",
            "--filter",
            "blob:none",
            "--branch",
            &max_version,
            full_url,
        ];

        let args = str_args
            .into_iter()
            .map(|s| s.as_ref())
            .chain(iter::once(download_root.as_ref()));

        self.execute_git(args, source, span, false)?;

        Some(install_path)
    }

    fn resolve_version(
        &mut self,
        source: VRef<Source>,
        version: Option<Span>,
        url: &str,
    ) -> Option<BumpVec<(u32, u32, u32)>> {
        let Some(version) = version else {
            return Some(bumpvec![]);
        };

        let version_str = &self.resources.sources[source].content[version.range()];
        let args = ["ls-remote", url, &format!("refs/tags/{}", version_str)]
            .into_iter()
            .map(|s| s.as_ref());

        let output = self.execute_git(args, source, version, true)?;

        let versions = output
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
            .collect::<BumpVec<_>>();

        Some(versions)
    }

    fn execute_git(
        &mut self,
        args: impl IntoIterator<Item = &OsStr> + Clone,
        source: VRef<Source>,
        loc: Span,
        quiet: bool,
    ) -> Option<String> {
        let command_str = args
            .clone()
            .into_iter()
            .map(|arg| arg.to_string_lossy())
            .intersperse(Cow::Borrowed(" "))
            .collect::<String>();

        if !quiet {
            self.git_info(source, loc, command_str);
        }

        let mut command = Command::new("git");
        command.args(args);
        let output = self.resources.db.command(&mut command);
        let output = output
            .map_err(|err| self.git_exec_error(source, loc, command_str, err))
            .ok()?;

        if !output.status.success() {
            let output = String::from_utf8_lossy(&output.stderr);
            self.git_exit_error(source, loc, command_str, output.as_ref());
            return None;
        }

        Some(String::from_utf8_lossy(&output.stdout).to_string())
    }

    gen_error_fns! {
        push invalid_package_root(self, root: &Path, err: io::Error) {
            err: "invalid root path";
            info: ("path: `{}`", root.display());
            info: ("trace: {}", err);
        }

        push non_dir_package_root(self, root: PathBuf) {
            err: "root path is not a directory";
            info: ("path: `{}`", root.display());
        }

        push unreachable_source(self, path: &Path, err: io::Error) {
            err: "failed to read source file";
            info: ("path: `{}`", path.display());
            info: ("trace: {}", err);
        }

        push invalid_module_path(self, path: &Path, err: io::Error) {
            err: "invalid module path";
            info: ("path: `{}`", path.display());
            info: ("trace: {}", err);
        }

        push non_file_module_path(self, path: PathBuf) {
            err: "module path is not a file";
            info: ("path: `{}`", path.display());
        }

        push invalid_dep_root_encoding(self, err: env::VarError) {
            warn: "invalid dep root";
            help: "path must be utf-8 encoded";
            info: ("trace: {}", err);
        }

        push invalid_manifest_root_field(self, field: ManifestFieldAst, source: VRef<Source>) {
            err: "invalid root field in manifest";
            (field.span(), source) {
                err[field.span()]: "expected string here";
            }
        }

        print git_info(self, source: VRef<Source>, span: Span, command: String) {
            info: ("executing git: {}", command);
            (span, source) {
                info[span]: "invocation declared here";
            }
        }

        push git_exec_error(self, source: VRef<Source>, span: Span, command: String, err: std::io::Error) {
            err: "git execution failed";
            info: ("executing git: {}", command);
            info: ("trace: {}", err);
            (span, source) {
                info[span]: "invocation declared here";
            }
        }

        push git_exit_error(self, source: VRef<Source>, span: Span, command: String, output: &str) {
            err: "git execution failed";
            info: ("executing git: {}", command);
            info: ("output:\n{}", output);
            (span, source) {
                info[span]: "invocation declared here";
            }
        }

        push invalid_version(self, source: VRef<Source>, span: Span) {
            warn: "invalid version";
            (span, source) {
                info[span]: "declared here";
            }
        }

        push invalid_dep_root(self, err: io::Error, path: &Path) {
            warn: "invalid dep root";
            info: ("path: `{}`", path.display());
            info: ("trace: {}", err);
        }

        push cannot_create_dep_root(self, err: io::Error, path: &Path) {
            warn: "cannot create dep root";
            info: ("path: `{}`", path.display());
            info: ("trace: {}", err);
        }

        push cannot_create_download_dir(self, err: io::Error, path: &Path) {
            warn: "cannot create download directory";
            info: ("path: `{}`", path.display());
            info: ("trace: {}", err);
        }
    }

    // fn load_low(&mut self, root: &Path, ctx: &mut Context) -> Option<()> {
    //     // setup
    //     let mut path = self
    //         .packages
    //         .resources
    //         .canonicalize(root)
    //         .map_err(|err| self.file_error(None, root, "manifest is missing", err))
    //         .ok()?;
    //     path.push("package");
    //     path.set_extension(MANIFEST_EXTENSION);
    //     let root_package_path = self.intern_path(&path)?;
    //     path.pop();
    //     let mut frontier = bumpvec![(root_package_path, path, default())];
    //     let mut ast_data = AstData::new();
    //     let mut parser_state = ParsingState::new();

    //     // load packages
    //     while !self.load_package(&mut ast_data, &mut parser_state, &mut frontier, root)? {}

    //     // load modules
    //     let Some(&ModKind::Package { ref root_module, span }) = self.packages.modules.get(&root_package_path).map(|m| &m.kind) else {
    //         unreachable!();
    //     };
    //     let path = root_module.to_owned();
    //     let module_id = self.load_modules(&path, root_package_path, span)?;

    //     // prepare cycle detection, we are doing packages and modules in one go
    //     self.package_graph.clear();
    //     self.package_graph
    //         .load_nodes(self.packages.modules.keys().map(|id| id.index() as u32));
    //     for (k, module) in self.packages.modules.iter() {
    //         self.package_graph.new_node(k.index() as u32).add_edges(
    //             self.packages.conns[module.deps]
    //                 .iter()
    //                 .map(|dep| dep.ptr.index() as u32),
    //         );
    //     }

    //     // detect cycles and build ordering
    //     let roots = [root_package_path.index() as u32, module_id.index() as u32];
    //     let mut ordering = bumpvec![cap self.packages.modules.len()];
    //     self.package_graph
    //         .ordering(roots, &mut ordering)
    //         .map_err(|cycle| self.dependency_cycle(cycle))
    //         .ok()?;

    //     // `SAFETY`: We previously supplied Ident internals into the graph
    //     // so it is safe to construct them from what graph gave us
    //     self.packages.module_order.extend(
    //         ordering
    //             .into_iter()
    //             .map(|i| unsafe { VRef::new(i as usize) })
    //             .filter(|id| self.packages.modules.get(id).unwrap().is_module()),
    //     );

    //     Some(())
    // }

    // /// Loads one package and pushes its dependencies to frontier.
    // /// Downloads packages if needed.
    // fn load_package(
    //     &mut self,
    //     ast_data: &mut AstData,
    //     parser_state: &mut ParsingState,
    //     frontier: &mut PackageFrontier,
    //     project_path: &Path,
    // ) -> Option<bool> {
    //     let Some((id, mut tmp_path, loc)) = frontier.pop() else {
    //         return Some(true);
    //     };

    //     // load content
    //     tmp_path.push("package");
    //     tmp_path.set_extension(MANIFEST_EXTENSION);
    //     let content = self
    //         .packages
    //         .resources
    //         .read_to_string(&tmp_path)
    //         .map_err(|err| self.file_error(loc, &tmp_path, "cannot load manifest", err))
    //         .ok()?;
    //     let path = tmp_path.clone();
    //     tmp_path.pop();

    //     // parse
    //     ast_data.clear();
    //     parser_state.start(&content, id);
    //     let manifest = {
    //         let mut ctx = ParsingCtx::new(
    //             &content,
    //             parser_state,
    //             ast_data,
    //             self.workspace,
    //             self.interner,
    //         );
    //         ManifestAst::parse(&mut ctx)
    //     };
    //     let Some(manifest) = manifest else {
    //         let package = Source {
    //             path,
    //             line_mapping: LineMapping::new(&content),
    //             content,
    //             ..default()
    //         };
    //         self.packages.modules.insert(id, package);
    //         return None;
    //     };

    //     // retrieve data from ast
    //     let (mut root_module, span) = {
    //         let root_ident = self.interner.intern_str("root");
    //         let (span, root_module) = manifest
    //             .find_field(root_ident)
    //             .and_then(|field| match field.value {
    //                 ManifestValueAst::String(span) => {
    //                     Some((span, &content[span.shrink(1).range()]))
    //                 }
    //                 _ => None,
    //             })
    //             .unwrap_or((default(), "root"));
    //         (tmp_path.join(root_module), span.into())
    //     };
    //     root_module.set_extension("");
    //     let deps = self.push_package_deps(
    //         id,
    //         &mut tmp_path,
    //         manifest.deps,
    //         &content,
    //         frontier,
    //         project_path,
    //     );

    //     // save
    //     let package = Source {
    //         path,
    //         line_mapping: LineMapping::new(&content),
    //         content,
    //     };
    //     self.packages.modules.insert(id, package);

    //     Some(false)
    // }

    // /// Function will find and optionally download all direct dependencies.
    // fn push_package_deps(
    //     &mut self,
    //     id: VRef<str>,
    //     temp_path: &mut PathBuf,
    //     deps_ast: ManifestDepsAst,
    //     content: &str,
    //     frontier: &mut PackageFrontier,
    //     project_path: &Path,
    // ) -> VSlice<Dep> {
    //     self.packages.conns.start_cache();
    //     for &ManifestDepAst {
    //         name,
    //         git,
    //         path,
    //         version,
    //         ..
    //     } in deps_ast.iter()
    //     {
    //         let path_str = &content[path.range()];

    //         let version_loc = version.map(|span| (id, span));
    //         let version = version.map(|span| &content[span.range()]);
    //         let current_loc = Some((id, path));

    //         // find the path of the dependency
    //         let path_result = if git {
    //             self.download_package(project_path, current_loc, version_loc, version, path_str)
    //         } else {
    //             let to_pop = Path::new(path_str).components().count();
    //             temp_path.push(path_str);
    //             let path = self
    //                 .packages
    //                 .resources
    //                 .canonicalize(temp_path)
    //                 .map_err(|err| {
    //                     self.file_error(
    //                         current_loc,
    //                         temp_path,
    //                         "cannot canonicalize local package path",
    //                         err,
    //                     )
    //                 });
    //             assert!((0..to_pop).fold(true, |acc, _| acc & temp_path.pop()));
    //             path.ok()
    //         };

    //         let Some(path) = path_result else {
    //             continue;
    //         };

    //         let Some(ptr) = self.intern_path(path.as_path()) else {
    //             continue;
    //         };

    //         // save
    //         let dep = Dep {
    //             name: name.ident,
    //             name_span: name.span,
    //             ptr,
    //         };
    //         self.packages.conns.cache(dep);

    //         if self.packages.modules.get(&ptr).is_none() {
    //             frontier.push((ptr, path, current_loc));
    //         }
    //     }

    //     self.packages.conns.bump_cached()
    // }

    // /// Only reachable modules are loaded.
    // fn load_modules(
    //     &mut self,
    //     path: &Path,
    //     package: VRef<str>,
    //     loc: Option<Span>,
    // ) -> Option<VRef<str>> {
    //     let loc = loc.map(|loc| (package, loc));

    //     let path = self
    //         .packages
    //         .resources
    //         .canonicalize(path)
    //         .map_err(|err| self.file_error(loc, path, "cannot canonicalize root module path", err))
    //         .ok()?
    //         .with_extension(FILE_EXTENSION);

    //     let id = self.intern_path(&path)?;

    //     let mut frontier = bumpvec![(id, package, path, default())];
    //     let mut ast_data = AstData::new();
    //     let mut parser_state = ParsingState::new();

    //     while !self.load_module(&mut ast_data, &mut parser_state, &mut frontier)? {}

    //     Some(id)
    // }

    // /// Returns true when last module was already loaded.
    // fn load_module(
    //     &mut self,
    //     ast_data: &mut AstData,
    //     parser_state: &mut ParsingState,
    //     frontier: &mut ModuleFrontier,
    // ) -> Option<bool> {
    //     let Some((id, package_id, path, loc)) = frontier.pop() else {
    //         return Some(true);
    //     };

    //     let content = self
    //         .packages
    //         .resources
    //         .read_to_string(&path)
    //         .map_err(|err| self.file_error(loc, &path, "cannot load source file", err))
    //         .ok()?;

    //     ast_data.clear();
    //     parser_state.start(&content, id);
    //     let imports = {
    //         let mut ctx = ParsingCtx::new(
    //             &content,
    //             parser_state,
    //             ast_data,
    //             self.workspace,
    //             self.interner,
    //         );
    //         UseAst::parse(&mut ctx)
    //     };

    //     let deps = if let Some(imports) = imports {
    //         self.load_module_deps(id, package_id, imports, &content, frontier)
    //             .unwrap_or_default()
    //     } else {
    //         default()
    //     };

    //     let module = Source {
    //         path,
    //         line_mapping: LineMapping::new(&content),
    //         content,
    //     };
    //     self.packages.modules.insert(id, module);

    //     Some(false)
    // }

    // fn load_module_deps(
    //     &mut self,
    //     id: VRef<str>,
    //     package_id: VRef<str>,
    //     imports: UseAst,
    //     content: &str,
    //     frontier: &mut ModuleFrontier,
    // ) -> Option<VSlice<Dep>> {
    //     self.packages.conns.start_cache();

    //     for &ImportAst { name, path, .. } in imports.items.iter() {
    //         let path_str = &content[path.range()];
    //         // $package$(/$module_path)?
    //         let (package, module_path) = path_str.split_once('/').unwrap_or((path_str, ""));
    //         let package_ident = self.interner.intern_str(package);

    //         // Try to find package mentioned by import, '.' is a self reference.
    //         let package_ent = &self.packages.modules.get(&package_id).unwrap();
    //         let maybe_package = self.packages.conns[package_ent.deps]
    //             .iter()
    //             .find_map(|dep| (dep.name == package_ident).then_some(dep.ptr))
    //             .or_else(|| (package == ".").then_some(package_id));
    //         let Some(external_package_id) = maybe_package else {
    //             let sippet = Self::unknown_package(self, path.sliced(..package.len()), id, package_ent);
    //             self.workspace.push(sippet);
    //             continue;
    //         };

    //         // All submodules of a package are expected to be in subdirectory with same name and level as
    //         // root module.
    //         let external_package = &self.packages.modules.get(&external_package_id).unwrap();
    //         let ModKind::Package { ref root_module, .. } = external_package.kind else {
    //             unreachable!();
    //         };
    //         let mut module_path = root_module.with_extension("").join(module_path);
    //         module_path.set_extension(FILE_EXTENSION);

    //         let import_loc = Some((id, path));

    //         let path = match self.packages.resources.canonicalize(&module_path) {
    //             Ok(path) => path,
    //             Err(err) => {
    //                 self.file_error(
    //                     import_loc,
    //                     &module_path,
    //                     "cannot canonicalize module path",
    //                     err,
    //                 );
    //                 continue;
    //             }
    //         };

    //         let import_id = self.intern_path(&path)?;

    //         let dep = Dep {
    //             name: name.ident,
    //             name_span: name.span,
    //             ptr: import_id,
    //         };
    //         self.packages.conns.cache(dep);

    //         if self.packages.modules.contains_key(&import_id) {
    //             continue;
    //         }

    //         frontier.push((import_id, external_package_id, path, import_loc));
    //     }

    //     Some(self.packages.conns.bump_cached())
    // }

    // /// git is invoked and package may be downloaded into `%CATALYST_CACHE%/url/(version || 'main')`
    // fn download_package(
    //     &mut self,
    //     project_path: &Path,
    //     source: VRef<Source>, span: Span,
    //     version_loc: DiagLoc,
    //     version: Option<&str>,
    //     url: &str,
    // ) -> Option<PathBuf> {
    //     let full_url = &format!("https://{}", url);
    //     let rev_owned = self.resolve_version(version_loc, version, full_url)?;
    //     if version.is_some() && rev_owned.is_none() {
    //         self.invalid_version(version_loc);
    //     }
    //     let rev = rev_owned.as_deref();

    //     let mut dep_root = self.get_dep_root(project_path);
    //     dep_root.push(url);
    //     dep_root.push(
    //         rev.map(|rev| &rev[..rev.find('.').unwrap_or(rev.len())])
    //             .unwrap_or("main"),
    //     );

    //     let exists = dep_root.exists();

    //     self.packages
    //         .resources
    //         .create_dir_all(&dep_root)
    //         .map_err(|err| self.file_error(loc, &dep_root, "cannot create directory", err))
    //         .ok()?;
    //     let install_path = self
    //         .packages
    //         .resources
    //         .canonicalize(&dep_root)
    //         .map_err(|err| {
    //             self.file_error(loc, &dep_root, "cannot canonicalize installation path", err)
    //         })
    //         .ok()?;
    //     if exists {
    //         return Some(install_path);
    //     }

    //     let id = self.intern_path(install_path.as_path())?;

    //     let fixed_args = ["clone", "--depth", "1", "--filter", "blob:none"];
    //     let str = self.interner[id].to_string();
    //     let paths = [full_url.as_str(), &str];
    //     let optional_args = rev.map(|rev| ["--branch", rev]);
    //     let args = fixed_args
    //         .into_iter()
    //         .chain(optional_args.into_iter().flatten())
    //         .chain(paths.into_iter());

    //     self.execute_git(loc, false, args)?;

    //     Some(install_path)
    // }

    // fn resolve_version(
    //     &mut self,
    //     source: VRef<Source>, span: Span,
    //     version: Option<&str>,
    //     url: &str,
    // ) -> Option<Option<String>> {
    //     let Some(version) = version else {
    //         return Some(None);
    //     };

    //     let args = ["ls-remote", url, &format!("refs/tags/{}", version)];

    //     let output = self.execute_git(loc, true, args)?;

    //     Some(
    //         output
    //             .lines()
    //             .filter_map(|line| line.split_whitespace().nth(1))
    //             .filter_map(|path| path.strip_prefix("refs/tags/"))
    //             .map(|version| {
    //                 version[1..]
    //                     .split('.')
    //                     .take(3)
    //                     .filter_map(|component| component.parse::<u32>().ok())
    //             })
    //             .filter_map(|mut comps| Some((comps.next()?, comps.next()?, comps.next()?)))
    //             .max()
    //             .map(|(major, minor, patch)| format!("v{}.{}.{}", major, minor, patch)),
    //     )
    // }

    // fn execute_git(
    //     &mut self,
    //     source: VRef<Source>, span: Span,
    //     quiet: bool,
    //     args: impl IntoIterator<Item = &str> + Clone,
    // ) -> Option<String> {
    //     if !quiet {
    //         self.git_info(loc, args.clone());
    //     }

    //     let mut command = Command::new("git");
    //     command.args(args.clone());
    //     let output = self.packages.resources.command(&mut command);
    //     let output = output
    //         .map_err(|err| self.git_exec_error(loc, args.clone(), err))
    //         .ok()?;

    //     if !output.status.success() {
    //         let output = String::from_utf8_lossy(&output.stderr);
    //         self.git_exit_error(loc, args, output.as_ref());
    //         return None;
    //     }

    //     Some(String::from_utf8_lossy(&output.stdout).to_string())
    // }

    // fn intern_path(&mut self, path: &Path) -> Option<VRef<str>> {
    //     let Some(str_path) = path.to_str() else {
    //         self.invalid_path_encoding(default(), path, "manifest");
    //         return None;
    //     };
    //     Some(self.interner.intern_str(str_path))
    // }

    // fn get_dep_root(&mut self, project_path: &Path) -> PathBuf {
    //     let var = self
    //         .packages
    //         .resources
    //         .var(DEP_ROOT_VAR)
    //         .unwrap_or_else(|_| DEFAULT_DEP_ROOT.into());
    //     project_path.join(var)
    // }

    // gen_error_fns! {
    //     push file_error(self, source: VRef<Source>, span: Span, path: &Path, message: &str, err: std::io::Error) {
    //         err: ("{}", message);
    //         info: ("related path: {}", path.display());
    //         info: ("trace: {}", err);
    //         (loc?.1, loc?.0) {
    //             err[loc?.1]: "caused by this";
    //         }
    //     }

    //     push invalid_path_encoding(self, source: VRef<Source>, span: Span, path: &Path, kind: &str) {
    //         err: ("invalid {} path encoding", kind);
    //         info: ("related path: {}", path.display());
    //         (loc?.1, loc?.0) {
    //             err[loc?.1]: "caused by this";
    //         }
    //     }

    //     push dependency_cycle(self, cycle: Vec<u32>) {
    //         err: "dependency cycle detected";
    //         info: (
    //             "cycle:\n{}",
    //             cycle
    //                 .iter()
    //                 .map(|&id| &self.interner[unsafe { VRef::<str>::new(id as usize) }])
    //                 .collect::<BumpVec<_>>()
    //                 .join("\n"),
    //         );
    //     }

    //     print git_info(self, source: VRef<Source>, span: Span, args: impl IntoIterator<Item = &str>) {
    //         info: ("executing git: {}", args.into_iter().collect::<BumpVec<_>>().join(" "));
    //         (loc?.1, loc?.0) {
    //             info[loc?.1]: "invocation declared here";
    //         }
    //     }

    //     push git_exec_error(self, source: VRef<Source>, span: Span, command: Stringerr: std::io::Error) {
    //         err: "git execution failed";
    //         info: ("executing git: {}", args.into_iter().collect::<BumpVec<_>>().join(" "));
    //         info: ("trace: {}", err);
    //         (loc?.1, loc?.0) {
    //             info[loc?.1]: "invocation declared here";
    //         }
    //     }

    //     push git_exit_error(self, source: VRef<Source>, span: Span, command: Strinoutput: &str) {
    //         err: "git execution failed";
    //         info: ("executing git: {}", args.into_iter().collect::<BumpVec<_>>().join(" "));
    //         info: ("output:\n{}", output);
    //         (loc?.1, loc?.0) {
    //             info[loc?.1]: "invocation declared here";
    //         }
    //     }

    //     push invalid_version(self, source: VRef<Source>, span: Span) {
    //         warn: "invalid version";
    //         (loc?.1, loc?.0) {
    //             info[loc?.1]: "version is specified here";
    //         }
    //     }

    //     unknown_package(s: &Self, span: Span, id: VRef<str>, package: &Source) {
    //         err: "cannot find this package in manifest";
    //         info: (
    //             "available packages: '{}'",
    //             s.packages.conns[package.deps]
    //                 .iter()
    //                 .map(|dep| &package.content[dep.name_span.range()])
    //                 .chain(std::iter::once("."))
    //                 .collect::<BumpVec<_>>()
    //                 .join("', '")
    //         );
    //         (span, id) {
    //             err[span]: "invalid package prefix used here";
    //         }
    //     }
    // }
}
