use std::path::PathBuf;

use lexer::*;
use storage::*;

pub type Units = PrimaryMap<Unit, UnitEnt>;

#[derive(Debug, Default)]
pub struct UnitEnt {
    pub local_source_path: PathBuf,
    pub root_path: PathBuf,
    pub source: Source,
}

impl UnitEnt {
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
