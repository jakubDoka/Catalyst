#![feature(let_else)]
#![feature(let_chains)]

use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{Signature, SourceLoc},
    isa::CallConv,
};
use cranelift_module::FuncId;
use std::{path::Path, time::SystemTime};
use storage::*;
// use typec_types::*;
use lexer::*;
use module_types::*;

pub type IncrFuncs = Map<IncrFunc>;
pub type IncrModules = Map<IncrModule>;

#[derive(Default)]
pub struct Incr {
    pub version: String,
    pub modules: IncrModules,
    pub functions: IncrFuncs,
    pub jit_functions: IncrFuncs,
}

impl Incr {
    pub fn load(version: String, path: &Path) -> Self {
        let s = Self::try_load(&version, path);
        if s.is_none() {
            println!("{INFO}info:{END} discarding incremental data");
        }
        let mut s = s.unwrap_or_default();
        s.version = version;
        s
    }

    pub fn try_load(version: &str, path: &Path) -> Option<Self> {
        let mut cursor = 0;
        let content = match std::fs::read(path) {
            Ok(c) => c,
            Err(err) => {
                println!("{WARNING}warning:{END} failed to read incremental data");
                println!("{INFO}trace:{END} {}", err);
                println!("{INFO}path searched:{END} {}", path.display());
                return Some(Self::default());
            }
        };

        let s = match Self::read(&mut cursor, &content) {
            Ok(s) => s,
            Err(err) => {
                println!("{WARNING}warning:{END} incremental data is corrupted");
                println!("{INFO}trace:{END} {}", err);
                return None;
            }
        };

        if s.version != version {
            println!(
                "{WARNING}warning:{END} incremental data is incompatible with current version"
            );
            return None;
        }
        Some(s)
    }

    pub fn save(&self, path: &Path) -> std::io::Result<()> {
        let mut contents = Vec::new();
        self.write(&mut contents);
        std::fs::write(path, contents)
    }

    /// we mark ans sweep garbage module and remove all related functions
    pub fn reduce(&mut self, modules: &Modules, module_order: &[Source]) {
        let mut dirty = EntitySet::new();
        let mut new_modules = Map::with_capacity(self.modules.len());
        
        for &id in module_order {
            let module = &modules[id];

            let Ok(Ok(modified)) = std::fs::metadata(&module.path).map(|m| m.modified()) else {
                dirty.insert(id);
                
                if let Some(mut existing) = self.modules.remove(id) {
                    Self::wipe(&mut existing, &mut self.functions);
                    Self::wipe(&mut existing, &mut self.jit_functions);
                    new_modules.insert(id, existing);
                }

                continue;
            };

            let Some(mut existing_module) = self.modules.remove(module.id) else {
                let new_module = IncrModule {
                    modified,
                    owned_functions: Map::new(),
                };
                new_modules.insert(module.id, new_module);
                continue;
            };

            if existing_module.modified != modified || module.dependency.iter().any(|&dep| dirty.contains(dep)) {
                dirty.insert(id);
                Self::wipe(&mut existing_module, &mut self.functions);
                Self::wipe(&mut existing_module, &mut self.jit_functions);
                existing_module.modified = modified;
                println!("gogaksdas;lkd;lakds;")
            }

            new_modules.insert(module.id, existing_module);
        }

        self.modules = new_modules;
    }

    pub fn wipe(module: &mut IncrModule, functions: &mut IncrFuncs) {
        for (id, _) in module.owned_functions.iter() {
            functions.remove(id);
        }

        module.owned_functions.clear();
    }
}

impl BitSerde for Incr {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.version.write(buffer);
        self.modules.write(buffer);
        self.functions.write(buffer);
        self.jit_functions.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        Ok(Self {
            version: String::read(cursor, buffer)?,
            modules: Map::read(cursor, buffer)?,
            functions: Map::read(cursor, buffer)?,
            jit_functions: Map::read(cursor, buffer)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncrModule {
    pub modified: SystemTime,
    pub owned_functions: Map<()>,
}

impl Default for IncrModule {
    fn default() -> Self {
        Self {
            modified: SystemTime::UNIX_EPOCH,
            owned_functions: Map::new(),
        }
    }
}

impl BitSerde for IncrModule {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.modified.write(buffer);
        self.owned_functions.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        Ok(Self {
            modified: SystemTime::read(cursor, buffer)?,
            owned_functions: Map::read(cursor, buffer)?,
        })
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct IncrFunc {
    pub signature: Signature,
    pub temp_id: Option<FuncId>,
    pub defined: bool,
    pub bytes: Vec<u8>,
    pub reloc_records: Vec<IncrRelocRecord>,
}

impl IncrFunc {
    pub fn dependencies(&self) -> impl Iterator<Item = ID> + '_ {
        self.reloc_records.iter().map(|r| r.name)
    }
}

impl Default for IncrFunc {
    fn default() -> Self {
        Self {
            temp_id: None,
            signature: Signature::new(CallConv::Fast),
            defined: false,
            bytes: Vec::new(),
            reloc_records: Vec::new(),
        }
    }
}

impl BitSerde for IncrFunc {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.signature.write(buffer);
        self.bytes.write(buffer);
        self.reloc_records.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        Ok(Self {
            signature: Signature::read(cursor, buffer)?,
            temp_id: None,
            defined: false,
            bytes: Vec::read(cursor, buffer)?,
            reloc_records: Vec::read(cursor, buffer)?,
        })
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct IncrRelocRecord {
    pub offset: CodeOffset,
    pub srcloc: SourceLoc,
    pub kind: Reloc,
    pub name: ID,
    pub addend: i64,
}

impl BitSerde for IncrRelocRecord {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.offset.write(buffer);
        self.srcloc.write(buffer);
        self.kind.write(buffer);
        self.name.write(buffer);
        self.addend.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Result<Self, String> {
        Ok(Self {
            offset: CodeOffset::read(cursor, buffer)?,
            srcloc: SourceLoc::read(cursor, buffer)?,
            kind: Reloc::read(cursor, buffer)?,
            name: ID::read(cursor, buffer)?,
            addend: i64::read(cursor, buffer)?,
        })
    }
}
