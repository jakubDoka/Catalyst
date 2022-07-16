use std::{
    fs::{create_dir_all, read_to_string},
    path::*,
    process::Command, str::FromStr,
};

use crate::*;
use diags::*;
use lexing::*;
use packaging_t::*;
use parsing::*;
use storage::*;

pub const MANIFEST_EXTENSION: &str = "ctlm";
pub const DEP_ROOT_VAR: &str = "CATALYST_DEP_ROOT";
pub const DEFAULT_DEP_ROOT: &str = "deps";

type Frontier = Vec<(Ident, PathBuf, Maybe<Loc>)>;

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
            if let Ok(true) = self.load_package(&mut ast_data, &mut parser_state, &mut frontier) {
                break;
            }
        }

        let ModuleKind::Package { ref root_module } = self.packaging_context.modules[id].kind else {
            unreachable!();
        };

        let path = root_module.to_owned();
        self.load_modules(&path)?;

        Ok(())
    }

    fn load_package(
        &mut self,
        ast_data: &mut AstData,
        parser_state: &mut ParserState,
        frontier: &mut Frontier,
    ) -> errors::Result<bool> {
        let Some((id, mut path, loc)) = frontier.pop() else {
            return Ok(true);
        };

        path.push("package");
        path.set_extension(MANIFEST_EXTENSION);

        let content = read_to_string(&path)
            .map_err(|err| self.file_error(loc, &path, "cannot load manifest", err))?;

        path.pop();

        parser_state.start(&content, id);
        let fields = Parser::new(&content, parser_state, ast_data, self.workspace).parse_manifest();

        let root_module = {
            let root_module = self
                .find_field_value("root", fields, &content, ast_data)
                .unwrap_or("root.ctl");
            path.join(root_module)
        };

        let deps =
            self.find_field("deps", fields, &content, ast_data)
                .map_or(Maybe::none(), |deps_ast| {
                    self.load_package_deps(id, &mut path, deps_ast, &content, ast_data, frontier)
                });

        let package = Module {
            path,
            line_mapping: LineMapping::new(&content),
            content,
            kind: ModuleKind::Package { root_module },
            deps,
        };
        self.packaging_context.modules.insert(id, package);

        Ok(false)
    }

    fn load_package_deps(
        &mut self,
        id: Ident,
        temp_path: &mut PathBuf,
        deps_ast: AstEnt,
        content: &str,
        ast_data: &AstData,
        frontier: &mut Frontier,
    ) -> Maybe<DepList> {
        self.packaging_context.conns.start_cache();
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

            let version = (version.kind != AstKind::None)
                .then_some(&content[version.span.range()]);

            let current_loc = Loc {
                source: id,
                span: path_span.into(),
            }
            .into();

            let path_result = if use_git {
                self.download_package(current_loc, version, path_str)
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
            self.packaging_context.conns.cache(dep);

            if self.packaging_context.modules.get(ptr).is_none() {
                frontier.push((ptr, path, current_loc));
            }

            // println!("{}", path.display());

        }

        self.packaging_context.conns.bump_cached()
    }

    fn load_modules(&mut self, path: &Path) -> errors::Result {
        // let id = self.intern_path(path)?;

        // let mut frontier = vec![(id, path.clone(), Maybe::none())];
        // let mut ast_data = AstData::new();
        // let mut parser_state = ParserState::new();

        // loop {
        //     if let Ok(true) = self.load_module(&mut ast_data, &mut parser_state, &mut frontier) {
        //         break;
        //     }
        // }

        Ok(())
    }

    fn download_package(&mut self, loc: Maybe<Loc>, version: Option<&str>, url: &str) -> errors::Result<PathBuf> {
        let url = &format!("https://{}", url);
        let rev_owned = self.resolve_version(loc, version, url)?;
        let rev = rev_owned.as_ref().map(|v| v.as_str());
        
        let mut dep_root = self.get_dep_root()?;
        dep_root.push(url);
        dep_root.push(rev.unwrap_or("main"));

        let install_path = dep_root.canonicalize().map_err(|err| {
            self.file_error(loc, &dep_root, "cannot canonicalize installation path", err)
        })?;

        create_dir_all(&install_path)
            .map_err(|err| self.file_error(loc, &dep_root, "cannot create directory", err))?;

        if install_path.exists() {
            return Ok(install_path);
        }

        let id = self.intern_path(install_path.as_path())?;

        let fixed_args = [
            "clone",
            "--depth",
            "1",
            "--filter",
            "blob:none",
            url,
            &self.interner[id].to_owned(),
        ];
        let optional_args = rev.map(|rev| ["--branch", rev]);
        let args = fixed_args
            .into_iter()
            .chain(optional_args.into_iter().flatten());

        self.execute_git(loc, args)?;

        Ok(install_path)
    }

    fn resolve_version(&mut self, loc: Maybe<Loc>, version: Option<&str>, url: &str) -> errors::Result<Option<String>> {
        let Some(version) = version else {
            return Ok(None);
        };

        let args = ["ls-remote", url, &format!("refs/tags/{}", version)]; 
        
        let output = self.execute_git(loc, args)?;

        
            
        todo!();
    }

    fn execute_git<'a>(&mut self, loc: Maybe<Loc>, args: impl IntoIterator<Item = &'a str> + Clone) -> errors::Result<String> {
        self.workspace.push(diag! {
            (exp loc) hint => "executing: git {}" {
                args.clone().into_iter().collect::<Vec<_>>().join(" ")
            }
        });

        let output = Command::new("git")
            .args(args)
            .output();
        let output = output.map_err(|err| {
            self.workspace.push(diag! {
                (exp loc) error => "cannot execute git",
                (none) => "error: {}" { err }
            })
        })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            self.workspace
                .push(diag!((exp loc) error => "git error: {}" { stderr }));
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

    fn get_dep_root(&mut self) -> errors::Result<PathBuf> {
        let var = std::env::var(DEP_ROOT_VAR).unwrap_or(DEFAULT_DEP_ROOT.into());
        if Path::new(&var).is_absolute() {
            Ok(PathBuf::from(var))
        } else {
            Path::new(&var)
                .canonicalize()
                .map_err(|err| self.invalid_dep_root_var(var, err))
        }
    }

    fn find_field_value<'a>(
        &self,
        name: &str,
        fields: Maybe<AstList>,
        content: &'a str,
        ast_data: &AstData,
    ) -> Option<&'a str> {
        self.find_field(name, fields, content, ast_data)
            .map(|ast| ast_data[ast.children][1].span)
            .map(|span| &content[span.range()])
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

    fn invalid_dep_root_var(&mut self, var: String, err: std::io::Error) {
        self.workspace.push(diag! {
            (none) error =>
            "invalid '{}', expected valid relative or absolute path to directory"
            { DEP_ROOT_VAR },
            (none) => "current value: {}" { var },
            (none) => "trace: {}" { err },
        });
    }
}

#[cfg(test)]
mod test {
    use std::env::var;

    use super::*;

    #[test]
    fn test_load() {
        let mut module_loader = PackageLoader {
            packaging_context: &mut PackagingContext::new(),
            workspace: &mut Workspace::new(),
            interner: &mut Interner::new(),
            package_graph: &mut PackageGraph::new(),
        };

        let path = var("TEST_PROJECT_PATH").expect("test should be run from `test.bat`");

        drop(module_loader.load(Path::new(&path)));

        module_loader.workspace.log(module_loader.packaging_context);

        assert!(!module_loader.workspace.has_errors());
    }
}
