#![feature(let_else)]
#![feature(let_chains)]

use cranelift_codegen::{
    binemit::{CodeOffset, Reloc},
    ir::{Signature, SourceLoc},
    isa::CallConv,
    MachReloc,
};
use cranelift_module::{FuncId, FuncOrDataId, Linkage, Module};
use std::{path::Path, time::SystemTime};
use storage::*;
// use typec_types::*;
use lexer_types::*;
use module_types::*;

#[derive(Default)]
pub struct Incr {
    pub version: String,
    pub modules: Map<IncrModule>,
    pub functions: Map<IncrFuncData>,
}

impl Incr {
    pub fn load(version: String, path: &Path) -> Self {
        let mut s = Self::try_load(&version, path).unwrap_or_default();
        s.version = version;
        s
    }

    pub fn try_load(version: &str, path: &Path) -> Option<Self> {
        let mut cursor = 0;
        let content = std::fs::read(path).ok()?;
        let s = Self::read(&mut cursor, &content)?;
        if s.version != version {
            return None;
        }
        Some(s)
    }

    pub fn save(&self, path: &Path) -> std::io::Result<()> {
        let mut contents = Vec::new();
        self.write(&mut contents);
        std::fs::write(path, contents)
    }

    pub fn reduce(&mut self, module_map: &Map<Source>, modules: &Modules, module_order: &[Source]) {
        let mut dirty = EntitySet::new();
        for (k, v) in self.modules.iter_mut() {
            let Some(&source) = module_map.get(k) else {
                Self::wipe(v, &mut self.functions);
                continue;
            };

            let module = &modules[source];
            let Ok(Ok(modified)) = std::fs::metadata(&module.path).map(|m| m.modified()) else {
                dirty.insert(source);
                Self::wipe(v, &mut self.functions);
                continue;
            };

            if v.modified != modified {
                dirty.insert(source);
                v.modified = modified;
                Self::wipe(v, &mut self.functions);
            }
        }

        // propagate wipes trough dependant modules
        for &module_id in module_order {
            if dirty.contains(module_id) {
                continue;
            }

            let module = &modules[module_id];
            let Some(incr_module) = self.modules.get_mut(module.id) else {
                unreachable!();
            };

            if module.dependency.iter().any(|&dep| dirty.contains(dep)) {
                dirty.insert(module_id);
                Self::wipe(incr_module, &mut self.functions)
            }
        }
    }

    pub fn wipe(module: &mut IncrModule, functions: &mut Map<IncrFuncData>) {
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
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Option<Self> {
        Some(Self {
            version: String::read(cursor, buffer)?,
            modules: Map::read(cursor, buffer)?,
            functions: Map::read(cursor, buffer)?,
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

    fn read(cursor: &mut usize, buffer: &[u8]) -> Option<Self> {
        Some(Self {
            modified: SystemTime::read(cursor, buffer)?,
            owned_functions: Map::read(cursor, buffer)?,
        })
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct IncrFuncData {
    pub signature: Signature,
    pub temp_id: Option<FuncId>,
    pub defined: bool,
    pub bytes: Vec<u8>,
    pub reloc_records: Vec<IncrRelocRecord>,
}

impl IncrFuncData {
    pub fn dependencies(&self) -> impl Iterator<Item = ID> + '_ {
        self.reloc_records.iter().map(|r| r.name)
    }
}

impl Default for IncrFuncData {
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

impl BitSerde for IncrFuncData {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.signature.write(buffer);
        self.bytes.write(buffer);
        self.reloc_records.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Option<Self> {
        Some(Self {
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

impl IncrRelocRecord {
    pub fn from_mach_reloc(reloc: &MachReloc, module: &impl Module) -> Self {
        let decl = module
            .declarations()
            .get_function_decl(FuncId::from_name(&reloc.name));

        let literal_name = if decl.linkage == Linkage::Import {
            decl.name.clone()
        } else {
            String::new()
        };

        let name = if literal_name.is_empty() {
            ID::from_ident(&decl.name)
        } else {
            ID::new(&literal_name)
        };

        Self {
            offset: reloc.offset,
            srcloc: reloc.srcloc,
            kind: reloc.kind,
            name,
            addend: reloc.addend,
        }
    }

    pub fn to_mach_reloc(&self, buffer: &mut String, module: &impl Module) -> MachReloc {
        buffer.clear();
        self.name.to_ident(buffer);

        let Some(FuncOrDataId::Func(func_id)) = module.get_name(&buffer) else {
            unreachable!()
        };

        MachReloc {
            offset: self.offset,
            srcloc: self.srcloc,
            kind: self.kind,
            name: func_id.into(),
            addend: self.addend,
        }
    }
}

impl BitSerde for IncrRelocRecord {
    fn write(&self, buffer: &mut Vec<u8>) {
        self.offset.write(buffer);
        self.srcloc.write(buffer);
        self.kind.write(buffer);
        self.name.write(buffer);
        self.addend.write(buffer);
    }

    fn read(cursor: &mut usize, buffer: &[u8]) -> Option<Self> {
        Some(Self {
            offset: CodeOffset::read(cursor, buffer)?,
            srcloc: SourceLoc::read(cursor, buffer)?,
            kind: Reloc::read(cursor, buffer)?,
            name: ID::read(cursor, buffer)?,
            addend: i64::read(cursor, buffer)?,
        })
    }
}
