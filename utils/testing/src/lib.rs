#![feature(closure_lifetime_binder)]
#![feature(let_else)]
#![feature(iter_intersperse)]

#[macro_export]
macro_rules! gen_test {
    (
        $test_struct:ty,
        $parallel:literal,
        $($($type:ident)? $name:literal $structure:tt)*
    ) => {
        std::thread::scope(|h| {
            fn testable<T: $crate::items::Testable>() {}
            testable::<$test_struct>();
            $(
                let value = $parallel.then_some(h);
                $crate::items::test_case($name, value, |name| {
                    let resources = gen_test!(__inner__ name $($type)? $structure);
                    <$test_struct>::new(resources).exec($name)
                });
            )*
        });
    };

    (__inner__ $name:ident simple $structure:tt) => {
        $crate::quick_file_system!(
            ($name)
            file "root.ctl" $structure
            file "package.ctlm" {}
        )
    };

    (__inner__ $name:ident {$($structure:tt)*}) => {
        $crate::quick_file_system!(
            ($name)
            $($structure)*
        )
    };
}

#[macro_export]
macro_rules! quick_file_system {
    (($root:expr) $($tokens:tt)*) => {
        {
            let mut __dir = $crate::items::Folder::new($root);
            $crate::quick_file_system!(__recur__ (__dir) $($tokens)*);
            __dir.create()
        }
    };

    (__recur__ ($parent:expr) $($key:ident $name:literal $content:tt)*) => {
        $(
            $crate::quick_file_system!(__item__ ($parent) $key $name $content);
        )*
    };

    (__item__ ($parent:expr) file $name:literal $content:tt) => {
        $parent.files.push(($name.to_string(), $crate::quick_file_system!(__file_content__ $content)));
    };

    (__file_content__ $content:literal) => {
        (false, $content.to_string())
    };

    (__file_content__ { $($content:tt)* }) => {
        (true, stringify!($($content)*).to_string())
    };

    (__item__ ($parent:expr) dir $name:literal {$($content:tt)*}) => {
        $crate::quick_file_system!(__item_low__ ($parent) false $name {$($content)*});
    };


    (__item__ ($parent:expr) remote_dir $name:literal {$($content:tt)*}) => {
        $crate::quick_file_system!(__item_low__ ($parent) true $name {$($content)*});
    };

    (__item_low__ ($parent:expr) $remote:literal $name:literal {$($content:tt)*}) => {
        let mut __dir = $crate::items::Folder::new($name);
        __dir.remote = $remote;
        $crate::quick_file_system!(__recur__ (__dir) $($content)*);
        $parent.folders.push(__dir);
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        quick_file_system!(
            ("foo")
            file "root.ctl" {
                fn main() {
                    "Hello, world!".log()
                }
            }
            file "package.ctlm" {
                root: "root.ctl";
            }
        );
    }
}

pub use items::Testable;

pub mod items {
    use diags::*;
    use fmt::Fmt;
    use packaging::Scheduler;
    use packaging_t::*;
    use snippet_display::SnippetDisplay;
    use std::{mem, thread::Scope};
    use storage::{Interner, VRef};

    use std::{
        collections::HashMap,
        env::VarError,
        io,
        path::{Path, PathBuf},
        process::{Command, CommandArgs, ExitStatus, Output},
    };

    impl<T: Scheduler + Default> Testable for T {
        fn exec(mut self, name: &str) -> (Workspace, Packages) {
            self.execute(Path::new(name));

            (
                mem::take(self.resources().workspace),
                mem::take(self.resources().packages),
            )
        }

        fn set_packages(&mut self, packages: Packages) {
            *self.resources().packages = packages;
        }
    }

    pub trait Testable: Default {
        fn exec(self, name: &str) -> (Workspace, Packages);
        fn set_packages(&mut self, packages: Packages);

        fn new(resources: TestResources) -> Self {
            let mut this = Self::default();
            let packages = Packages::with_resources(resources);
            this.set_packages(packages);
            this
        }
    }

    pub fn test_case<'a: 'b, 'b, 'c>(
        name: &'static str,
        scope: Option<&'a Scope<'b, 'c>>,
        test_code: fn(&str) -> (Workspace, Packages),
    ) {
        let runner = move || {
            let (mut ws, packages) = test_code(name);

            let out = ws.display(&packages, &mut SnippetDisplay::default());

            let path = format!("{}/{}.txt", "test_out", name);
            if !Path::new("test_out").exists() {
                std::fs::create_dir("test_out").unwrap();
            }
            std::fs::write(path, out).unwrap();
        };

        if let Some(scope) = scope {
            scope.spawn(runner);
        } else {
            runner();
        }
    }

    pub struct Folder {
        pub remote: bool,
        pub name: String,
        pub files: Vec<(String, (bool, String))>,
        pub folders: Vec<Folder>,
    }

    impl Folder {
        pub fn new(name: &str) -> Self {
            Self {
                remote: false,
                name: name.to_string(),
                files: Vec::new(),
                folders: Vec::new(),
            }
        }

        pub fn create(self) -> TestResources {
            let mut resources = TestResources::default();
            let mut packages = Packages::default();
            let mut interner = Interner::new();

            let mut fmt = Fmt::default();
            let path = PathBuf::from(&self.name);
            self.create_recur(
                &path,
                &mut fmt,
                &mut interner,
                &mut packages,
                &mut resources,
            );

            let str = fmt
                .workspace
                .display(&packages, &mut SnippetDisplay::default());
            let path = format!("test_out/{}-parse-out.txt", path.display());

            if str.trim() != "" {
                std::fs::write(path, str).unwrap();
            } else if Path::new(&path).exists() {
                std::fs::remove_file(path).unwrap();
            }

            resources
        }

        fn create_recur(
            mut self,
            path: &Path,
            fmt: &mut Fmt,
            interner: &mut Interner,
            packages: &mut Packages,
            resources: &mut TestResources,
        ) {
            let self_path = path;
            if !path.exists() {
                resources.create_dir_all(self_path).unwrap();
            }
            for (name, (replace, content)) in self.files.drain(..) {
                let formatter = if name.ends_with(".ctlm") {
                    Fmt::manifest
                } else if name.ends_with(".ctl") {
                    Fmt::source
                } else {
                    for<'a> |_: &'a mut Fmt, s: String, _: VRef<str>| -> (Option<&'a str>, String) {
                        (None, s)
                    }
                };

                let path = self_path.join(name);
                let path_ident = interner.intern_str(path.to_str().unwrap());

                let c = if replace {
                    content.replace('\n', " ").replace("::", "`")
                } else {
                    content
                };

                let (res, c) = formatter(fmt, c, path_ident);

                let res = res.unwrap_or(&c);

                resources.add_file(&path, res.to_string());

                let module = Mod {
                    path,
                    content: c,
                    ..Default::default()
                };
                packages.modules.insert(path_ident, module);
            }

            for mut folder in self.folders.drain(..) {
                if folder.remote {
                    let name = std::mem::take(&mut folder.name);
                    let res = folder.create();
                    resources.add_remote(name, res);
                } else {
                    let path = path.join(&folder.name);
                    folder.create_recur(&path, fmt, interner, packages, resources);
                }
            }
        }
    }

    #[derive(Default)]
    pub struct TestResources {
        pub env: HashMap<String, String>,
        pub files: HashMap<PathBuf, String>,
        pub binary_files: HashMap<PathBuf, Vec<u8>>,
        pub repositories: HashMap<String, TestResources>,
    }

    fn new_exist_status(code: u32) -> ExitStatus {
        #[cfg(windows)]
        return std::os::windows::process::ExitStatusExt::from_raw(code);

        #[cfg(unix)]
        return std::os::unix::process::ExitStatusExt::from_raw(code);
    }

    impl TestResources {
        pub fn add_file(&mut self, path: &Path, content: String) {
            self.files.insert(self.canonicalize(path).unwrap(), content);
        }

        pub fn add_binary_file(&mut self, path: &Path, content: Vec<u8>) {
            self.binary_files
                .insert(self.canonicalize(path).unwrap(), content);
        }

        pub fn add_remote(&mut self, key: String, resources: Self) -> &mut Self {
            self.repositories.entry(key).or_insert(resources)
        }

        pub fn execute_git(&mut self, args: CommandArgs) -> io::Result<Output> {
            let args = args
                .map(|arg| arg.to_str())
                .collect::<Option<Vec<_>>>()
                .ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidInput, "Invalid arguments encoding")
                })?;

            let subcommand_index = 0;
            match args.get(subcommand_index).copied() {
                Some("clone") => {
                    let &[ref others @ .., mut repository, destination] = args.as_slice() else {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput, "expected arguments: <destination> <repository>"));
                    };

                    repository = repository.strip_prefix("https://").ok_or_else(|| {
                        io::Error::new(io::ErrorKind::InvalidInput, "expected https:// repository")
                    })?;

                    let branch = others
                        .iter()
                        .skip_while(|arg| arg != &&"--branch")
                        .nth(1)
                        .copied()
                        .unwrap_or("main");
                    let repository = format!("{}#{}", repository, branch);

                    let Some(repository) = self.repositories.get(&repository) else {
                        return Ok(Output {
                            status: new_exist_status(128),
                            stdout: Vec::new(),
                            stderr: format!("fatal: repository '{}' not found", repository).into_bytes(),
                        });
                    };

                    let destination = PathBuf::from(destination);
                    for (path, file) in repository.files.iter() {
                        let path = self.canonicalize(&destination.join(path))?;
                        self.files.insert(path, file.clone());
                    }
                    for (path, file) in repository.binary_files.iter() {
                        let path = self.canonicalize(&destination.join(path))?;
                        self.binary_files.insert(path, file.clone());
                    }
                    Ok(Output {
                        status: new_exist_status(0),
                        stderr: Vec::new(),
                        stdout: "Repository downloaded".to_string().into(),
                    })
                }
                Some("ls-remote") => {
                    let &[_, mut repository, _] = args.as_slice() else {
                        return Err(io::Error::new(io::ErrorKind::InvalidInput, "expected arguments: <repository> <pattern>"));
                    };

                    repository = repository.strip_prefix("https://").ok_or_else(|| {
                        io::Error::new(io::ErrorKind::InvalidInput, "expected https:// repository")
                    })?;

                    let res = self
                        .repositories
                        .keys()
                        .filter_map(|key| key.strip_prefix(&format!("{repository}#")))
                        .map(|branch| format!("9821309128301928302193\trefs/tags/{}\n", branch))
                        .collect::<String>();

                    if res.is_empty() {
                        return Ok(Output {
                            status: new_exist_status(128),
                            stdout: Vec::new(),
                            stderr: format!("fatal: repository '{}' not found", repository)
                                .into_bytes(),
                        });
                    }

                    Ok(Output {
                        status: new_exist_status(0),
                        stderr: Vec::new(),
                        stdout: res.into(),
                    })
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Expected subcommand: ls-remote | clone",
                )),
            }
        }
    }

    impl Resources for TestResources {
        fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
            self.binary_files
                .get(self.canonicalize(path)?.as_path())
                .cloned()
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
        }

        fn exists(&self, path: &Path) -> bool {
            self.binary_files.contains_key(path) || self.files.contains_key(path)
        }

        fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
            let mut canon_path = PathBuf::with_capacity(path.as_os_str().len());
            let mut iter = path.components().peekable();
            loop {
                let Some(current) = iter.next() else {
                    break Ok(canon_path);
                };

                let Some(next) = iter.peek() else {
                    canon_path.push(current);
                    break Ok(canon_path);
                };

                if next.as_os_str() == Path::new("..") {
                    iter.next();
                } else {
                    canon_path.push(current);
                }
            }
        }

        fn command(&mut self, command: &mut Command) -> io::Result<Output> {
            match command.get_program().to_str() {
                Some("git") => self.execute_git(command.get_args()),
                _ => Err(io::Error::new(io::ErrorKind::NotFound, "command not found")),
            }
        }

        fn read_to_string(&self, path: &Path) -> io::Result<String> {
            self.files
                .get(self.canonicalize(path)?.as_path())
                .cloned()
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
        }

        fn var(&self, key: &str) -> Result<String, VarError> {
            self.env.get(key).cloned().ok_or(VarError::NotPresent)
        }

        fn create_dir_all(&self, _: &Path) -> io::Result<()> {
            Ok(()) // nothing
        }
    }
}
