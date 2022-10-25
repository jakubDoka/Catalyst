use std::{
    env::{self, VarError},
    fs, io,
    path::{Path, PathBuf},
    process::{Command, Output},
    time::SystemTime,
};

pub trait ResourceDb: Send + Sync + 'static {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>>;
    fn exists(&self, path: &Path) -> bool;
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
    fn command(&mut self, command: &mut Command) -> io::Result<Output>;
    fn get_modification_time(&self, path: &Path) -> io::Result<SystemTime>;
    fn read_to_string(&self, path: &Path) -> io::Result<String>;
    fn var(&self, key: &str) -> Result<String, VarError>;
    fn create_dir_all(&self, path: &Path) -> io::Result<()>;
}

pub struct OsResources;

impl ResourceDb for OsResources {
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

    fn get_modification_time(&self, path: &Path) -> io::Result<SystemTime> {
        fs::metadata(path).and_then(|metadata| metadata.modified())
    }
}

impl Default for Box<dyn ResourceDb> {
    fn default() -> Self {
        Box::new(OsResources)
    }
}
