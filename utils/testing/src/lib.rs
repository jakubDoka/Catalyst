#![feature(closure_lifetime_binder)]
#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(once_cell)]
#![feature(internal_output_capture)]

#[macro_export]
macro_rules! gen_test {
    (
        $test_struct:ty,
        $parallel:literal,
        $($($type:ident)? $name:literal $structure:tt)*
    ) => {
        $crate::fmt::QuickTimer::set_enabled(false);
        let tests = [$({
            let name = $name;
            (name, gen_test!(__inner__ name $($type)? $structure))
        },)*];
        $crate::items::run::<$test_struct, _>(tests, $parallel);
    };

    (__inner__ $name:ident simple $structure:tt) => {
        $crate::quick_file_system!(
            ($name)
            file "root.ctl" $structure
            file "package.ctlm" {
                deps { git "github.com/jakubDoka/water" }
            }
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

pub use {fmt, items::Testable};

pub mod items {
    use diags::*;
    use fmt::{FmtRuntime, FmtRuntimeCtx, Middleware};
    use resources::*;
    use snippet_display::SnippetDisplayImpl;
    use std::iter;
    use std::sync::{Arc, LazyLock, Mutex};
    use std::time::SystemTime;

    use std::{
        collections::HashMap,
        env::VarError,
        io,
        path::{Path, PathBuf},
        process::{Command, CommandArgs, ExitStatus, Output},
    };

    pub trait Testable: Default {
        fn exec<'a>(
            &'a mut self,
            name: &str,
            middleware: &'a mut Middleware,
            resources: &'a mut TestResources,
        ) -> (&'a mut Workspace, &'a Resources);
    }

    pub fn run<T, I>(tests: I, parallel: bool)
    where
        T: Testable + Send,
        I: IntoIterator<Item = (&'static str, TestResources)>,
        I::IntoIter: Send,
    {
        let thread_count = std::thread::available_parallelism()
            .ok()
            .filter(|_| parallel)
            .map(|x| x.get())
            .unwrap_or(1);

        #[derive(Default)]
        struct Thread<T> {
            testable: T,
            mid: Middleware,
            fmt: FmtRuntimeCtx,
            stdout: Arc<Mutex<Vec<u8>>>,
        }

        let mut threads = iter::repeat_with(Thread::<T>::default)
            .take(thread_count)
            .collect::<Vec<_>>();

        let queue = Mutex::new(tests.into_iter());

        std::thread::scope(|scope| {
            for thread in threads.iter_mut() {
                scope.spawn(|| {
                    while let Ok(mut handle) = queue.lock() && let Some((name, mut task)) = handle.next() {
                        drop(handle);

                        task.add_water();

                        //io::set_output_capture(Some(thread.stdout.clone()));

                        FmtRuntime::test_format(name, &mut thread.mid, &mut thread.fmt, &mut task);
                        let (ws, res) = thread.testable.exec(name, &mut thread.mid, &mut task);

                        io::set_output_capture(None);
                        if let Ok(mut handle) = thread.stdout.lock() {
                            let message = format!("Test {name} stdout:\n{}", String::from_utf8_lossy(&handle));
                            handle.clear();
                            println!("{}", message);
                        }

                        let mut out = String::new();
                        ws.display(&res, &mut SnippetDisplayImpl::default(), &mut out);
                        ws.clear();
                        let path = format!("{}/{}.txt", "test_out", name);
                        if !Path::new("test_out").exists() {
                            std::fs::create_dir("test_out").unwrap();
                        }
                        std::fs::write(path, out).unwrap();
                    }
                });
            }
        })
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
            let name = self.name.clone();
            let path = PathBuf::from(&name);
            self.create_recur(&path, &mut resources);
            resources
        }

        fn create_recur(mut self, path: &Path, resources: &mut TestResources) {
            let self_path = path;
            if !path.exists() {
                resources.create_dir_all(self_path).unwrap();
            }
            for (name, (replace, content)) in self.files.drain(..) {
                let path = self_path.join(name);

                let res = if replace {
                    content
                        .replace('\n', " ")
                        .replace(" :: ", "\\")
                        .replace(":: ", "\\")
                } else {
                    content
                };

                resources.add_file(&path, res);
            }

            for mut folder in self.folders.drain(..) {
                if folder.remote {
                    let name = std::mem::take(&mut folder.name);
                    let res = folder.create();
                    resources.add_remote(name, res);
                } else {
                    let path = path.join(&folder.name);
                    folder.create_recur(&path, resources);
                }
            }
        }
    }

    #[derive(Default, Debug)]
    pub struct TestResources {
        pub env: HashMap<String, String>,
        pub files: HashMap<PathBuf, (String, SystemTime)>,
        pub binary_files: HashMap<PathBuf, (Vec<u8>, SystemTime)>,
        pub repositories: HashMap<String, Arc<TestResources>>,
    }

    fn new_exist_status(code: u32) -> ExitStatus {
        #[cfg(windows)]
        return std::os::windows::process::ExitStatusExt::from_raw(code);

        #[cfg(unix)]
        return std::os::unix::process::ExitStatusExt::from_raw(code as i32);
    }

    impl TestResources {
        pub fn add_water(&mut self) {
            static WATER_REPO: LazyLock<Arc<TestResources>> =
                LazyLock::new(|| Arc::new(TestResources::water_repo()));

            self.repositories.insert(
                "github.com/jakubDoka/water#main".to_string(),
                WATER_REPO.clone(),
            );
        }

        pub fn water_repo() -> Self {
            Self {
                files: [
                    ("root.ctl", include_str!("../../../water/root.ctl")),
                    ("package.ctlm", include_str!("../../../water/package.ctlm")),
                    ("root/vec.ctl", include_str!("../../../water/root/vec.ctl")),
                    ("root/ptr.ctl", include_str!("../../../water/root/ptr.ctl")),
                    ("root/c.ctl", include_str!("../../../water/root/c.ctl")),
                    (
                        "root/macros/tokens.ctl",
                        include_str!("../../../water/root/macros/tokens.ctl"),
                    ),
                ]
                .map(|(path, content)| {
                    (
                        PathBuf::from(path),
                        (content.to_string(), SystemTime::now()),
                    )
                })
                .into(),
                ..Default::default()
            }
        }

        pub fn add_file(&mut self, path: &Path, content: String) {
            self.files.insert(
                self.canonicalize(path).unwrap(),
                (content, SystemTime::now()),
            );
        }

        pub fn add_binary_file(&mut self, path: &Path, content: Vec<u8>) {
            self.binary_files.insert(
                self.canonicalize(path).unwrap(),
                (content, SystemTime::now()),
            );
        }

        pub fn add_remote(&mut self, key: String, resources: Self) -> &mut Self {
            Arc::get_mut(self.repositories.entry(key).or_insert(Arc::new(resources))).unwrap()
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
                    let repository = format!("{repository}#{branch}");

                    let Some(repository) = self.repositories.get(&repository) else {
                        return Ok(Output {
                            status: new_exist_status(128),
                            stdout: Vec::new(),
                            stderr: format!("fatal: repository {repository:?} not found").into_bytes(),
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
                        .map(|branch| format!("9821309128301928302193\trefs/tags/{branch}\n"))
                        .collect::<String>();

                    if res.is_empty() {
                        return Ok(Output {
                            status: new_exist_status(128),
                            stdout: Vec::new(),
                            stderr: format!("fatal: repository '{repository}' not found")
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

    impl ResourceDb for TestResources {
        fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
            self.binary_files
                .get(self.canonicalize(path)?.as_path())
                .cloned()
                .map(|(data, _)| data)
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
        }

        fn write_to_string(&mut self, path: &Path, data: &str) -> io::Result<()> {
            self.files
                .insert(
                    self.canonicalize(path)?,
                    (data.to_owned(), SystemTime::now()),
                )
                .unwrap();
            Ok(())
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
                .map(|(data, _)| data)
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
        }

        fn var(&self, key: &str) -> Result<String, VarError> {
            self.env.get(key).cloned().ok_or(VarError::NotPresent)
        }

        fn create_dir_all(&self, _: &Path) -> io::Result<()> {
            Ok(()) // nothing
        }

        fn get_modification_time(&self, path: &Path) -> io::Result<std::time::SystemTime> {
            Ok(self
                .files
                .get(self.canonicalize(path)?.as_path())
                .map(|(_, time)| *time)
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))?)
        }
    }
}
