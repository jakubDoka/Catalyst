use std::{
    collections::HashMap,
    env::{self, VarError},
    fs, io,
    path::{Path, PathBuf},
    process::{Command, CommandArgs, ExitStatus, Output},
};

pub trait Resources {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>>;
    fn exists(&self, path: &Path) -> bool;
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
    fn command(&mut self, command: &mut Command) -> io::Result<Output>;
    fn read_to_string(&self, path: &Path) -> io::Result<String>;
    fn var(&self, key: &str) -> Result<String, VarError>;
    fn create_dir_all(&self, path: &Path) -> io::Result<()>;
}

pub struct OsResources;

impl Resources for OsResources {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        fs::read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        path.canonicalize()
    }

    fn command(&mut self, command: &mut Command) -> io::Result<Output> {
        command.output()
    }

    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }

    fn var(&self, key: &str) -> Result<String, VarError> {
        env::var(key)
    }

    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        fs::create_dir_all(path)
    }
}

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
    pub fn execute_git(&mut self, args: CommandArgs) -> io::Result<Output> {
        let args = args
            .map(|arg| arg.to_str())
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "Invalid arguments encoding")
            })?;

        let subcommand_index = 1;
        match args.get(subcommand_index).copied() {
            Some("clone") => {
                let &[ref others @ .., repository, destination] = args.as_slice() else {
                    return Err(io::Error::new(io::ErrorKind::InvalidInput, "expected arguments: <destination> <repository>"));
                };

                let branch = others
                    .iter()
                    .skip_while(|arg| arg != &&"--branch")
                    .skip(1)
                    .next()
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
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Expected subcommand: ls-files | clone",
                ))
            }
        }

        Ok(Output {
            status: new_exist_status(0),
            stdout: Vec::new(),
            stderr: Vec::new(),
        })
    }
}

impl Resources for TestResources {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.binary_files
            .get(self.canonicalize(path)?.as_path())
            .map(|v| v.clone())
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
    }

    fn exists(&self, path: &Path) -> bool {
        self.binary_files.contains_key(path) || self.files.contains_key(path)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        Ok(path
            .to_str()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid path encoding"))?
            .replace('\\', "/")
            .split('/')
            .filter(|s| !s.is_empty())
            .intersperse("/")
            .collect::<String>()
            .into())
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
            .map(|v| v.clone())
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
    }

    fn var(&self, key: &str) -> Result<String, VarError> {
        self.env
            .get(key)
            .map(|v| v.clone())
            .ok_or_else(|| VarError::NotPresent)
    }

    fn create_dir_all(&self, _: &Path) -> io::Result<()> {
        Ok(()) // nothing
    }
}

impl Default for Box<dyn Resources> {
    fn default() -> Self {
        Box::new(OsResources)
    }
}
