use std::{
    borrow::Cow, collections::hash_map::Entry, default::default, env::VarError, ffi::OsStr, io,
    iter, mem, path::*, process::Command, time::SystemTime,
};

use diags::*;
use lexing::*;
use parsing::*;
use resources::*;
use storage::*;

const DEP_ROOT_VAR: &str = "CATALYST_DEP_ROOT";

#[derive(Default)]
pub struct ResourceLoaderCtx {
    sources: Map<PathBuf, VRef<Source>>,
    packages: Map<PathBuf, DummyPackage>,
    modules: Map<PathBuf, DummyModule>,
    package_frontier: Vec<(PathBuf, Option<SourceLoc>, bool)>,
    module_frontier: Vec<(PathBuf, VRef<Package>, VRef<Source>, Span)>,
    ast_arena: Option<Arena>,
    dep_root: PathBuf,
}

impl ResourceLoaderCtx {
    fn get_ast_data(&mut self) -> Arena {
        self.ast_arena.take().unwrap_or_default()
    }

    fn clear(&mut self, resources: &mut Resources, db: &mut dyn ResourceDb) {
        self.sources.clear();
        resources
            .sources
            .retain(|source| source.ensure_loaded(db).is_ok());
        resources
            .sources
            .iter()
            .map(|(key, source)| (source.path.to_owned(), key))
            .collect_into(&mut self.sources);
        self.packages.clear();
        self.modules.clear();
        self.package_frontier.clear();
        self.module_frontier.clear();
        if let Some(ref mut ast_data) = self.ast_arena {
            ast_data.clear();
        }
        self.sources.insert(
            Resources::BUILTIN_SOURCE_PATH.into(),
            Resources::BUILTIN_SOURCE,
        );
    }
}

struct DummyPackage {
    root_module: PathBuf,
    root_module_span: Span,
    deps: Vec<(NameAst, PathBuf)>,
    source: VRef<Source>,
    ordering: usize,
    is_external: bool,
}

#[derive(Debug)]
struct DummyModule {
    package: VRef<Package>,
    deps: Vec<(Option<Vis>, NameAst, PathBuf)>,
    source: VRef<Source>,
    ordering: usize,
}

pub struct PackageLoader<'ctx> {
    pub resources: &'ctx mut Resources,
    pub workspace: &'ctx mut Workspace,
    pub interner: &'ctx mut Interner,
    pub package_graph: &'ctx mut PackageGraph,
    pub db: &'ctx mut dyn ResourceDb,
}

impl<'ctx> PackageLoader<'ctx> {
    /// Loads the project into graph of manifests and source files.
    pub fn reload(
        &mut self,
        root_path: &Path,
        ctx: &mut ResourceLoaderCtx,
    ) -> Option<Vec<VRef<Source>>> {
        ctx.clear(self.resources, self.db);
        self.resources.clear();

        ctx.dep_root = self.resolve_dep_root_path(root_path)?;
        let root_path = self.resolve_source_path(root_path, None, "package")?;
        let root_package = self.load_packages(root_path, ctx)?;

        let mut buffer = bumpvec![cap self.resources.packages.len()];
        self.package_graph.clear();
        for package in self.resources.packages.values() {
            let edges = self.resources.package_deps[package.deps]
                .iter()
                .map(|dep| dep.ptr.index());
            self.package_graph.new_node().add_edges(edges)
        }
        self.package_graph
            .ordering(iter::once(root_package.index()), &mut buffer)
            .map_err(|cycle| {
                self.workspace.push(CycleDetected {
                    cycle: cycle
                        .into_iter()
                        .map(VRef::<Package>::new)
                        .map(|package| self.resources.packages[package].source)
                        .map(|source| self.resources.sources[source].path.to_string_lossy())
                        .intersperse(Cow::Borrowed("\n"))
                        .collect::<String>(),
                    something: "package",
                })
            })
            .ok();
        buffer.clear();

        let root_module = self.load_modules(root_package, ctx)?;

        self.package_graph.clear();
        for module in self.resources.modules.values() {
            let edges = self.resources.module_deps[module.deps]
                .iter()
                .map(|dep| dep.ptr.index());
            self.package_graph.new_node().add_edges(edges)
        }

        self.package_graph
            .ordering(root_module.map(|i| i.index()), &mut buffer)
            .map_err(|cycle| {
                self.workspace.push(CycleDetected {
                    cycle: cycle
                        .into_iter()
                        .map(VRef::<Module>::new)
                        .map(|package| self.resources.modules[package].source)
                        .map(|source| self.resources.sources[source].path.to_string_lossy())
                        .intersperse(Cow::Borrowed("\n"))
                        .collect::<String>(),
                    something: "module",
                })
            })
            .ok();

        self.resources
            .module_order
            .extend(buffer.drain(..).map(VRef::new));
        self.resources.mark_changed();

        self.resources
            .module_order
            .iter()
            .enumerate()
            .for_each(|(i, &id)| self.resources.modules[id].ordering = i);

        let to_remove = self
            .resources
            .sources
            .iter()
            .filter_map(|(key, source)| source.dead.then_some(key))
            .collect::<Vec<_>>();

        for &key in &to_remove {
            let package = self.resources.sources.remove(key);
            ctx.sources.remove(&package.path);
        }

        let changed = self
            .resources
            .sources
            .iter()
            .filter_map(|(k, s)| s.changed.then_some(k));

        Some(to_remove.into_iter().chain(changed).collect::<Vec<_>>()).filter(|v| !v.is_empty())
    }

    fn load_modules(
        &mut self,
        package: VRef<Package>,
        ctx: &mut ResourceLoaderCtx,
    ) -> Option<[VRef<Module>; 2]> {
        ctx.modules
            .entry(Resources::BUILTIN_SOURCE_PATH.into())
            .or_insert_with(|| DummyModule {
                package: Resources::BUILTIN_PACKAGE,
                deps: default(),
                source: Resources::BUILTIN_SOURCE,
                ordering: 0,
            });

        let &Package {
            ref root_module,
            source,
            root_module_span,
            ..
        } = &self.resources.packages[package];
        ctx.module_frontier
            .push((root_module.clone(), package, source, root_module_span));

        let mut ast_data = ctx.get_ast_data();
        while let Some((path, package, source, span)) = ctx.module_frontier.pop() {
            self.load_module(path, package, source, span, &mut ast_data, ctx);
        }
        ctx.ast_arena = Some(ast_data);

        for module in ctx.modules.values_mut() {
            let final_module = Module {
                package: module.package,
                ordering: 0,
                deps: default(),
                source: module.source,
            };

            module.ordering = self.resources.modules.push(final_module).index();
        }

        for (module, dummy_module) in self
            .resources
            .modules
            .values_mut()
            .zip(ctx.modules.values())
        {
            let deps_iter = dummy_module
                .deps
                .iter()
                .filter_map(|&(vis, name, ref path)| {
                    let index = ctx.modules.get(path)?.ordering;
                    Some(Dep {
                        vis,
                        name_span: name.span,
                        name: name.ident,
                        ptr: VRef::<Module>::new(index),
                    })
                });
            module.deps = self.resources.module_deps.extend(deps_iter);
        }

        let getter = |package: VRef<Package>| {
            let path = &self.resources.packages[package].root_module;
            ctx.modules.get(path).map(|m| m.ordering).map(VRef::new)
        };

        Some([getter(package)?, getter(Resources::BUILTIN_PACKAGE)?])
    }

    fn load_module(
        &mut self,
        path: PathBuf,
        package: VRef<Package>,
        origin: VRef<Source>,
        span: Span,
        ast_data: &mut Arena,
        ctx: &mut ResourceLoaderCtx,
    ) -> Option<()> {
        let source = self.load_source(
            path.clone(),
            Some(SourceLoc { origin, span }),
            package,
            ctx,
            "module",
        )?;

        let content = &self.resources.sources[source].content;
        let mut parser_ctx = ParserCtx::<NoTokenMeta>::new(content);
        ast_data.clear();

        let (imports, ..) = Parser::new(
            self.interner,
            self.workspace,
            &mut parser_ctx,
            ast_data,
            source,
            content,
        )
        .imports()?;

        let deps = imports
            .map(|imports| self.resolve_module_deps(imports, package, source, ctx))
            .unwrap_or_default();

        let dummy_module = DummyModule {
            package,
            deps,
            source,
            ordering: 0,
        };
        ctx.modules.insert(path, dummy_module);

        Some(())
    }

    fn resolve_module_deps(
        &mut self,
        imports: ImportsAst,
        package_id: VRef<Package>,
        origin: VRef<Source>,
        ctx: &mut ResourceLoaderCtx,
    ) -> Vec<(Option<Vis>, NameAst, PathBuf)> {
        imports
            .items
            .iter()
            .filter_map(|&ImportAst { vis, name, path }| {
                let path = path.span.shrink(1);
                let name = self.extract_path_name("module", path, name, origin)?;
                let path_content = self.resources.sources[origin].span_str(path);
                let (package, path_str) =
                    path_content.split_once('/').unwrap_or((path_content, ""));

                let package_ident = self.interner.intern(package);
                let import_package = self.resources.package_deps
                    [self.resources.packages[package_id].deps]
                    .iter()
                    .find_map(|dep| (dep.name == package_ident).then_some(dep.ptr))
                    .or_else(|| {
                        Some(match package {
                            "." => package_id,
                            "builtin" => Resources::BUILTIN_PACKAGE,
                            _ => return None,
                        })
                    });

                let Some(import_package) = import_package else {
                    self.workspace.push(UnknownPackage {
                        packages: self.resources.package_deps
                            [self.resources.packages[package_id].deps]
                            .iter()
                            .map(|dep| dep.name.get(self.interner))
                            .intersperse(", ")
                            .collect::<String>(),
                        loc: SourceLoc { origin, span: path.sliced(..package.len()) },
                    })?;
                };

                let built_path = self.resources.packages[import_package]
                    .root_module
                    .with_extension("")
                    .join(path_str)
                    .with_extension("ctl");

                if import_package == Resources::BUILTIN_PACKAGE {
                    return Some((vis.map(|vis| vis.vis), name, built_path));
                }

                let dep_path = match self.db.canonicalize(&built_path) {
                    Ok(p) => p,
                    Err(trace) => self.workspace.push(InvalidDefinedPath {
                        path: built_path,
                        trace,
                        loc: SourceLoc { origin, span: path }.into(),
                        something: "module",
                    })?,
                };

                if !ctx.modules.contains_key(&dep_path) {
                    ctx.module_frontier
                        .push((dep_path.clone(), import_package, origin, path));
                }

                Some((vis.map(|vis| vis.vis), name, dep_path))
            })
            .collect()
    }

    fn resolve_dep_root_path(&mut self, root_path: &Path) -> Option<PathBuf> {
        let path = match self.db.var(DEP_ROOT_VAR) {
            Ok(path) if Path::new(&path).is_absolute() => path.into(),
            Ok(path) => root_path.join(path),
            Err(VarError::NotPresent) => {
                let default_path = root_path.join("deps");
                self.db
                    .create_dir_all(&default_path)
                    .map_err(|trace| {
                        self.workspace.push(PathRelatedError {
                            path: default_path.clone(),
                            trace,
                            message: "failed to create default dependency root",
                        })
                    })
                    .ok()?;
                default_path
            }
            Err(..) => self.workspace.push(InvalidDepRootEncoding {})?,
        };

        let root = self
            .db
            .canonicalize(&path)
            .map_err(|err| PathRelatedError {
                path,
                trace: err,
                message: "failed to canonicalize dependency root",
            })
            .ok()?;

        Some(root)
    }

    fn load_packages(
        &mut self,
        root_path: PathBuf,
        ctx: &mut ResourceLoaderCtx,
    ) -> Option<VRef<Package>> {
        ctx.package_frontier.push((root_path.clone(), None, false));
        let mut ast_data = ctx.get_ast_data();
        while let Some((path, loc, is_external)) = ctx.package_frontier.pop() {
            self.load_package(path, loc, &mut ast_data, is_external, ctx);
        }
        ctx.ast_arena = Some(ast_data);

        for package in ctx.packages.values_mut() {
            let final_package = Package {
                root_module: mem::take(&mut package.root_module),
                root_module_span: package.root_module_span,
                deps: default(),
                source: package.source,
                is_external: package.is_external,
            };
            let id = self.resources.packages.push(final_package);
            package.ordering = id.index();
        }

        let iter = self
            .resources
            .packages
            .values_mut()
            .skip(1) // builtin package
            .zip(ctx.packages.values());
        for (package, dummy_package) in iter {
            let deps_iter = dummy_package
                .deps
                .iter()
                .filter_map(|(name, path)| {
                    let index = ctx.packages.get(path)?.ordering;
                    Some(Dep {
                        vis: None,
                        name_span: name.span,
                        name: name.ident,
                        ptr: VRef::<Package>::new(index),
                    })
                })
                .chain(iter::once(Dep {
                    vis: None,
                    name_span: default(),
                    name: Interner::BUILTIN,
                    ptr: Resources::BUILTIN_PACKAGE,
                }));
            package.deps = self.resources.package_deps.extend(deps_iter);
        }

        let package = root_path.join("package").with_extension("ctlm");
        let root_package = ctx.packages.get(&package)?;
        Some(VRef::new(root_package.ordering))
    }

    fn load_package(
        &mut self,
        path: PathBuf,
        loc: Option<SourceLoc>,
        ast_data: &mut Arena,
        is_external: bool,
        ctx: &mut ResourceLoaderCtx,
    ) -> Option<()> {
        let package_path = path.join("package").with_extension("ctlm");

        let source_id = self.load_source(
            package_path,
            loc,
            Resources::BUILTIN_PACKAGE,
            ctx,
            "package",
        )?;

        let source = &self.resources.sources[source_id];
        let mut parser_ctx = ParserCtx::new(&source.content);
        ast_data.clear();

        let manifest = Parser::new(
            self.interner,
            self.workspace,
            &mut parser_ctx,
            ast_data,
            source_id,
            &source.content,
        )
        .manifest()?;

        let (root_module, root_module_span) =
            self.resolve_root_module_path(&path, source_id, manifest)?;
        let deps = manifest
            .find_deps()
            .flat_map(|deps| deps.list.iter().copied());
        let deps = self.resolve_manifest_deps(&path, source_id, deps, ctx);

        let package = DummyPackage {
            root_module,
            deps,
            source: source_id,
            ordering: 0,
            root_module_span,
            is_external,
        };
        ctx.packages
            .insert(self.resources.sources[source_id].path.clone(), package);

        Some(())
    }

    fn resolve_manifest_deps(
        &mut self,
        root_path: &Path,
        origin: VRef<Source>,
        deps: impl Iterator<Item = ManifestDepAst>,
        ctx: &mut ResourceLoaderCtx,
    ) -> Vec<(NameAst, PathBuf)> {
        deps.filter_map(|dep| {
            let ManifestDepAst {
                git,
                name,
                path: ast_path,
                version,
            } = dep;

            let ast_path = ast_path.span.shrink(1);

            let name = self.extract_path_name("package", ast_path, name, origin)?;

            let path = if git.is_some() {
                self.download_package(origin, version, ast_path, ctx)
            } else {
                let path_content = self.resources.sources[origin].span_str(ast_path);
                let package_path = root_path.join(path_content);
                self.resolve_source_path(
                    &package_path,
                    Some(SourceLoc {
                        origin,
                        span: ast_path,
                    }),
                    "package",
                )
            }?;

            let manifest = path.join("package").with_extension("ctlm");
            if !ctx.packages.contains_key(&manifest) {
                ctx.package_frontier.push((
                    path,
                    Some(SourceLoc {
                        origin,
                        span: ast_path,
                    }),
                    git.is_some(),
                ));
            }
            Some((name, manifest))
        })
        .collect()
    }

    fn extract_path_name(
        &mut self,
        _for_the: &'static str,
        path: Span,
        name: Option<NameAst>,
        origin: VRef<Source>,
    ) -> Option<NameAst> {
        if name.is_some() {
            return name;
        }

        let path_str = self.resources.sources[origin].span_str(path);
        let index = path_str.rfind('/').map(|i| i + 1).unwrap_or(0);

        Some(NameAst {
            ident: self.interner.intern(&path_str[index..]),
            source_info: SourceInfo {
                span: path.sliced(index..),
                meta: NoTokenMeta,
            },
        })
    }

    fn resolve_root_module_path(
        &mut self,
        root_path: &Path,
        origin: VRef<Source>,
        manifest: ManifestAst,
    ) -> Option<(PathBuf, Span)> {
        let field_name = self.interner.intern("root");
        let value_span = manifest
            .find_field(field_name)
            .and_then(|root| match root.value {
                ManifestValueAst::String(str) => Some(str),
                value => self.workspace.push(InvalidManifestRootField {
                    loc: SourceLoc {
                        origin,
                        span: value.span(),
                    },
                })?,
            });

        let content = &self.resources.sources[origin].content;

        let name = value_span.map_or("root", |str| &content[str.span.shrink(1).range()]);
        let loc = value_span.map(|span| SourceLoc {
            origin,
            span: span.span,
        });
        let full_path = root_path.join(name).with_extension("ctl");
        self.resolve_source_path(&full_path, loc, "root module")
            .map(|path| (path, value_span.map_or(default(), |value| value.span)))
    }

    fn load_source(
        &mut self,
        path: PathBuf,
        loc: Option<SourceLoc>,
        package: VRef<Package>,
        ctx: &mut ResourceLoaderCtx,
        owner: &'static str,
    ) -> Option<VRef<Source>> {
        let last_modified = self.db.get_modification_time(&path);
        if let Some(&source) = ctx.sources.get(&path)
            && let Ok(last_modified) = last_modified
            && let source_ent = &mut self.resources.sources[source]
            && source_ent.last_modified == last_modified
        {
            if source_ent.dead {
                source_ent.changed = false;
                source_ent.dead = false;
            }
            source_ent.package = package;
            return Some(source);
        }

        let content = self
            .db
            .read_to_string(&path)
            .map_err(|trace| {
                self.workspace.push(InvalidDefinedPath {
                    loc,
                    path: path.clone(),
                    trace,
                    something: owner,
                })
            })
            .ok()?;
        let last_modified = last_modified.unwrap_or_else(|_| SystemTime::now());

        let source = Source {
            path: path.clone(),
            last_modified,
            line_mapping: LineMapping::new(&content),
            content,
            changed: true,
            dead: false,
            loaded: true,
            builtin: false,
            package,
        };

        Some(match ctx.sources.entry(path) {
            Entry::Occupied(entry) => {
                let &source_id = entry.get();
                self.resources.sources[source_id] = source;
                source_id
            }
            Entry::Vacant(entry) => {
                let source_id = self.resources.sources.push(source);
                entry.insert(source_id);
                source_id
            }
        })
    }

    fn resolve_source_path(
        &mut self,
        root: &Path,
        loc: Option<SourceLoc>,
        owner: &'static str,
    ) -> Option<PathBuf> {
        self.db
            .canonicalize(root)
            .map_err(|err| {
                self.workspace.push(InvalidDefinedPath {
                    loc,
                    path: root.to_owned(),
                    trace: err,
                    something: owner,
                })
            })
            .ok()
    }

    /// git is invoked and package may be downloaded into `%CATALYST_CACHE%/url/(version || 'main')`
    fn download_package(
        &mut self,
        origin: VRef<Source>,
        version: Option<SourceInfo>,
        url_span: Span,
        ctx: &ResourceLoaderCtx,
    ) -> Option<PathBuf> {
        let url = self.resources.sources[origin].span_str(url_span);
        let full_url = &format!("https://{url}");
        let version = version
            .map(|v| self.resolve_version(origin, v.span, full_url))
            .transpose()?
            .unwrap_or_else(|| "main".to_string());

        let url = self.resources.sources[origin].span_str(url_span);
        let download_root = ctx.dep_root.join(url).join(&version);

        let exists = download_root.exists();

        self.db
            .create_dir_all(&download_root)
            .map_err(|err| {
                self.workspace.push(InvalidDefinedPath {
                    loc: Some(SourceLoc {
                        origin,
                        span: url_span,
                    }),
                    path: download_root.clone(),
                    trace: err,
                    something: "package",
                })
            })
            .ok()?;

        let install_path = self
            .db
            .canonicalize(&download_root)
            .expect("we just created it, there is no reason for this to fail");

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
            &version,
            full_url,
        ];

        let args = str_args
            .into_iter()
            .map(|s| s.as_ref())
            .chain(iter::once(download_root.as_ref()));

        self.execute_git(args, origin, url_span)?;

        Some(install_path)
    }

    fn resolve_version(
        &mut self,
        origin: VRef<Source>,
        version: Span,
        url: &str,
    ) -> Option<String> {
        const TAG_PREFIX: &str = "refs/tags/";

        let version_str = &self.resources.sources[origin].content[version.shrink(1).range()];
        let tag_pattern = format!("{TAG_PREFIX}{version_str}");
        let args = ["ls-remote", url, &tag_pattern]
            .into_iter()
            .map(|s| s.as_ref());

        let output = self.execute_git(args, origin, version)?;

        let opt_version = output
            .lines()
            .filter_map(|line| line.split_whitespace().nth(1))
            .filter_map(|path| path.strip_prefix(TAG_PREFIX))
            .filter_map(|version| version.strip_prefix('v'))
            .map(|version| {
                version
                    .split('.')
                    .take(3)
                    .filter_map(|component| component.parse::<u32>().ok())
            })
            .filter_map(|mut comps| Some((comps.next()?, comps.next()?, comps.next()?)))
            .max();

        let Some((major, minor, patch)) = opt_version else {
            self.workspace.push(InvalidVersion {
                url: url.to_owned(),
                loc: SourceLoc { origin, span: version },
            })?;
        };

        Some(format!("v{major}.{minor}.{patch}"))
    }

    fn execute_git(
        &mut self,
        args: impl IntoIterator<Item = &OsStr> + Clone,
        origin: VRef<Source>,
        span: Span,
    ) -> Option<String> {
        let command_str = args
            .clone()
            .into_iter()
            .map(|arg| arg.to_string_lossy())
            .intersperse(Cow::Borrowed(" "))
            .collect::<String>();

        let mut command = Command::new("git");
        command.args(args);
        let output = self.db.command(&mut command);
        let output = output
            .map_err(|err| {
                self.workspace.push(GitExecError {
                    command: command_str.clone(),
                    loc: SourceLoc { origin, span },
                    err,
                })
            })
            .ok()?;

        if !output.status.success() {
            let output = String::from_utf8_lossy(&output.stderr);
            self.workspace.push(GitExitError {
                command: command_str,
                loc: SourceLoc { origin, span },
                output: output.to_string(),
            })?;
        }

        Some(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

ctl_errors! {
    #[err => "missing {something} name"]
    error MissingName: fatal {
        #[err loc]
        something: &'static str,
        loc: SourceLoc,
    }

    #[err => "git exited with non-zero status code"]
    #[info => "args: `{command}`"]
    #[info => "stderr: {output}"]
    error GitExitError: fatal {
        #[err loc]
        command ref: String,
        loc: SourceLoc,
        output ref: String,
    }

    #[err => "failed to execute git command"]
    #[info => "args: `{command}`"]
    #[info => "error: {err}"]
    error GitExecError: fatal {
        #[err loc]
        command ref: String,
        loc: SourceLoc,
        err ref: io::Error,
    }

    #[err => "invalid version of dependency"]
    #[info => "the version did not match any tag in '{url}' repository"]
    error InvalidVersion: fatal {
        #[err loc]
        url ref: String,
        loc: SourceLoc,
    }

    #[err => "unknown package"]
    #[info => "available packages: {packages}"]
    #[note => "to refer to current package use '.'"]
    error UnknownPackage: fatal {
        #[err loc]
        packages ref: String,
        loc: SourceLoc,
    }

    #[err => "{something} cycle detected"]
    #[info => ("cycle:\n{cycle}")]
    error CycleDetected: fatal {
        cycle ref: String,
        something: &'static str,
    }

    #[err => "invalid {something} path"]
    #[info => ("path searched: `{}`", path.display())]
    #[info => ("exact io error: {}", trace)]
    error InvalidDefinedPath: fatal {
        #[err loc, "derived from this"]
        path ref: PathBuf,
        trace ref: io::Error,
        loc: Option<SourceLoc>,
        something: &'static str,
    }

    #[err => "{message}"]
    #[info => ("path searched: `{}`", path.display())]
    #[info => ("exact io error: {}", trace)]
    error PathRelatedError: fatal {
        path ref: PathBuf,
        trace ref: io::Error,
        message: &'static str,
    }

    #[err => "'{DEP_ROOT_VAR}' exists but has invalid encoding"]
    #[note => "path must be utf-8 encoded"]
    error InvalidDepRootEncoding: fatal {}

    #[err => "invalid 'root' field in manifest"]
    #[note => "expected string"]
    error InvalidManifestRootField: fatal {
        #[err loc]
        loc: SourceLoc,
    }

    #[err => "this code feels like desert"]
    #[info => "project needs to have exactly one 'moist' dependency"]
    #[help => r#"add 'git "github.com/jakubDoka/water" "*"' to the 'deps {{}}' sections in 'project.ctlm'"#]
    #[help => "alternatively you can add 'moist: true' into 'project.ctlm'"]
    error NoMoisture: fatal {}

    #[err => "your code is too wet"]
    #[info => "project can have only one 'moist' dependency"]
    #[info => "first 'moist' dependency is at {first:?}"]
    #[info => "second 'moist' dependency is at {second:?}"]
    error TooMoist: fatal {
        first ref: PathBuf,
        second ref: PathBuf,
    }
}
