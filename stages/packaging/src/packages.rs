use std::{
    fs::{create_dir_all, read_to_string},
    path::*,
    process::Command,
};

use crate::*;
use diags::*;
use lexing::*;
use packaging_t::*;
use parsing::*;
use storage::*;

pub const MANIFEST_EXTENSION: &str = ".ctlm";
pub const DEP_ROOT_VAR: &str = "CATALYST_DEP_ROOT";
pub const DEFAULT_DEP_ROOT: &str = "deps";

type Frontier = Vec<(Ident, PathBuf, Maybe<Loc>)>;

impl PackageLoader<'_> {
    pub fn load(&mut self, root: &Path) -> errors::Result {
        let mut path = root.to_owned();
        path.push("package");
        path.set_extension(MANIFEST_EXTENSION);
        let id = self.intern_path(&path)?;

        let mut frontier = vec![(id, root.to_owned(), Maybe::none())];
        let mut ast_data = AstData::new();
        let mut parser_state = ParserState::new();

        loop {
            if let Ok(true) = self.load_package(&mut ast_data, &mut parser_state, &mut frontier) {
                break;
            }
        }

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

        let Some(deps_ast) = self.find_field("deps", fields, &content, ast_data) else {
            return Ok(false);
        };

        let deps = self.load_package_deps(id, &mut path, deps_ast, &content, ast_data, frontier);

        let package = Module {
            path: root_module,
            line_mapping: LineMapping::new(&content),
            content,
            ordering: 0,
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
        frontier: &mut Frontier
    ) -> Maybe<DepList> {
        self.packaging_context.conns.start_cache();
        for dep in &ast_data[deps_ast.children] {
            let &AstEnt { kind: AstKind::ManifestImport { use_git }, children, .. } = dep else {
                unreachable!();
            };

            let [name, path] = &ast_data[children] else {
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

            let current_loc = Loc {
                source: id,
                span: path_span.into(),
            }
            .into();

            let path_result = if use_git {
                self.download_package(current_loc, path_str)
            } else {
                temp_path.push(path_str);
                temp_path.canonicalize().map_err(|err| {
                    self.file_error(
                        current_loc,
                        temp_path,
                        "cannot canonicalize local package path",
                        err,
                    )
                })
            };

            let Ok(path) = path_result else {
                continue;
            };

            let Ok(ptr) = self.intern_path(path.as_path()) else {
                continue;
            };

            let dep = Dep { name, ptr };
            self.packaging_context.conns.cache(dep);

            frontier.push((ptr, path, current_loc));
        }

        self.packaging_context.conns.bump_cached()
    }

    fn download_package(&mut self, loc: Maybe<Loc>, path: &str) -> errors::Result<PathBuf> {
        let (url, rev) = path
            .split_once('@')
            .map(|(url, rev)| (url, Some(rev)))
            .unwrap_or((path, None));

        let rev = rev.map(|rev| {
            rev.parse::<Version>()
                .ok()
                .map(|v| v.major().to_string())
                .unwrap_or_else(|| rev.to_string())
        });

        let mut dep_root = self.get_dep_root()?;
        dep_root.push(url);
        dep_root.push(rev.as_ref().map(|s| s.as_str()).unwrap_or("main"));

        let install_path = dep_root.canonicalize().map_err(|err| {
            self.file_error(loc, &dep_root, "cannot canonicalize installation path", err)
        })?;

        create_dir_all(&install_path)
            .map_err(|err| self.file_error(loc, &dep_root, "cannot create directory", err))?;

        if install_path.exists() {
            return Ok(install_path);
        }

        let id = self.intern_path(install_path.as_path())?;

        let fixed_args = ["clone", "--depth", "1", url, &self.interner[id]];
        let optional_args = rev.as_ref().map(|rev| ["--branch", rev.as_str()]);
        let args = fixed_args
            .into_iter()
            .chain(optional_args.into_iter().flatten());

        let output_result = Command::new("git").args(args.clone()).output();

        self.workspace
            .push(diag!((exp loc) hint => "executing: git {}" {
                args.collect::<Vec<_>>().join(" ")
            }));

        let output = output_result.map_err(|err| {
            self.workspace.push(diag!(
                (exp loc) error => "cannot execute git",
                (none) => "error: {}" { err }
            ))
        })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            self.workspace
                .push(diag!((exp loc) error => "git error: {}" { stderr }));
            return Err(());
        }

        Ok(install_path)
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
            .find(|field| &content[field.span.range()] == name)
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
