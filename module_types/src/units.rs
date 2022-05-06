use std::path::PathBuf;

use lexer_types::*;
use storage::*;

pub type Units = PrimaryMap<Unit, Ent>;

#[derive(Debug, Default)]
pub struct Ent {
    pub local_source_path: PathBuf,
    pub root_path: PathBuf,
    pub source: Source,
}

impl Ent {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_absolute_source_path(&self) -> std::io::Result<PathBuf> {
        self.root_path
            .join(self.local_source_path.as_path())
            .canonicalize()
    }
}

gen_entity!(Unit);