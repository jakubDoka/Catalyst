use std::fmt::Display;

use ::object::{RelocationEncoding, RelocationKind, SymbolScope};
use cranelift_codegen::{binemit::Reloc, isa::TargetIsa};
use object::{
    write::{Object, Relocation, StandardSection, Symbol, SymbolId, SymbolSection},
    SymbolFlags, SymbolKind,
};
use storage::{BumpVec, Interner, ShadowMap, VRef};
use typec_t::{FuncVisibility, Typec};

use crate::{CompiledFunc, Gen, GenReloc};

#[derive(Clone, Copy)]
struct ObjectFunction {
    symbol: SymbolId,
}

pub struct ObjectContext {
    functions: ShadowMap<CompiledFunc, Option<ObjectFunction>>,
    object: Object<'static>,
}

impl ObjectContext {
    pub fn new(isa: &dyn TargetIsa) -> Result<Self, ObjectCreationError> {
        let triple = isa.triple();
        let binary_format = match triple.binary_format {
            target_lexicon::BinaryFormat::Elf => object::BinaryFormat::Elf,
            target_lexicon::BinaryFormat::Coff => object::BinaryFormat::Coff,
            target_lexicon::BinaryFormat::Macho => object::BinaryFormat::MachO,
            target_lexicon::BinaryFormat::Wasm => object::BinaryFormat::Wasm,
            other => Err(ObjectCreationError::UnsupportedBinaryFormat(other))?,
        };

        let architecture = match triple.architecture {
            target_lexicon::Architecture::X86_32(_) => object::Architecture::I386,
            target_lexicon::Architecture::X86_64 => object::Architecture::X86_64,
            target_lexicon::Architecture::Arm(_) => object::Architecture::Arm,
            target_lexicon::Architecture::Aarch64(_) => object::Architecture::Aarch64,
            target_lexicon::Architecture::S390x => object::Architecture::S390x,
            other => Err(ObjectCreationError::UnsupportedArchitecture(other))?,
        };

        let endian = match triple
            .endianness()
            .map_err(|()| ObjectCreationError::MissingEndianness)?
        {
            target_lexicon::Endianness::Little => object::Endianness::Little,
            target_lexicon::Endianness::Big => object::Endianness::Big,
        };

        Ok(Self {
            functions: ShadowMap::new(),
            object: Object::new(binary_format, architecture, endian),
        })
    }

    pub fn load_functions(
        &mut self,
        funcs: &[VRef<CompiledFunc>],
        gen: &Gen,
        typec: &Typec,
        interner: &Interner,
    ) {
        let filtered_funcs = funcs
            .iter()
            .filter_map(|&func| self.functions[func].is_none().then_some(func))
            .collect::<BumpVec<_>>();

        for &func in filtered_funcs.iter() {
            let ent = &gen.compiled_funcs[func];

            let id = gen.compiled_funcs.id(func);
            let (_, name) = &interner[id].split_once('\\').unwrap_or(("", &interner[id]));

            let meta = &typec.funcs[ent.func];
            let (scope, weak) = Self::translate_visibility(meta.visibility);

            let symbol = self.object.add_symbol(Symbol {
                name: name.as_bytes().to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Text,
                scope,
                weak,
                section: SymbolSection::Undefined,
                flags: SymbolFlags::None,
            });

            self.functions[func] = Some(ObjectFunction { symbol })
        }

        let text_section = self.object.section_id(StandardSection::Text);

        for &func in filtered_funcs.iter() {
            let ent = &gen.compiled_funcs[func];
            let obj_fn = self.functions[func].as_mut().unwrap();

            self.object
                .add_symbol_data(obj_fn.symbol, text_section, &ent.bytecode, ent.alignment);
        }

        for &func in filtered_funcs.iter() {
            let ent = &gen.compiled_funcs[func];
            let obj_fn = self.functions[func].as_mut().unwrap();

            let offset = self
                .object
                .symbol_section_and_offset(obj_fn.symbol)
                .unwrap()
                .1;
            for &record in &ent.relocs {
                let reloc = self.process_reloc(record, offset);
                self.object
                    .add_relocation(text_section, reloc)
                    .expect("Relocation should be valid.");
            }
        }
    }

    fn process_reloc(&self, record: GenReloc, symbol_offset: u64) -> Relocation {
        macro_rules! format_assert {
            ($format:ident, $variant:ident) => {
                assert_eq!(
                    self.object.format(),
                    object::BinaryFormat::$format,
                    "{} is not supported for this file format",
                    stringify!($variant)
                );
            };
        }

        let mut addend = record.addend as i64;
        let (kind, encoding, size) = match record.kind {
            Reloc::Abs4 => (RelocationKind::Absolute, RelocationEncoding::Generic, 32),
            Reloc::Abs8 => (RelocationKind::Absolute, RelocationEncoding::Generic, 64),
            Reloc::X86PCRel4 => (RelocationKind::Relative, RelocationEncoding::Generic, 32),
            Reloc::X86CallPCRel4 => (RelocationKind::Relative, RelocationEncoding::X86Branch, 32),
            // TODO: Get Cranelift to tell us when we can use
            // R_X86_64_GOTPCRELX/R_X86_64_REX_GOTPCRELX.
            Reloc::X86CallPLTRel4 => (
                RelocationKind::PltRelative,
                RelocationEncoding::X86Branch,
                32,
            ),
            Reloc::X86SecRel => (
                RelocationKind::SectionOffset,
                RelocationEncoding::Generic,
                32,
            ),
            Reloc::X86GOTPCRel4 => (RelocationKind::GotRelative, RelocationEncoding::Generic, 32),
            Reloc::Arm64Call => (
                RelocationKind::Relative,
                RelocationEncoding::AArch64Call,
                26,
            ),
            Reloc::ElfX86_64TlsGd => {
                format_assert!(Elf, ElfX86_64TlsGd);
                (
                    RelocationKind::Elf(object::elf::R_X86_64_TLSGD),
                    RelocationEncoding::Generic,
                    32,
                )
            }
            Reloc::MachOX86_64Tlv => {
                format_assert!(MachO, MachOX86_64Tlv);
                addend += 4; // X86_64_RELOC_TLV has an implicit addend of -4
                (
                    RelocationKind::MachO {
                        value: object::macho::X86_64_RELOC_TLV,
                        relative: true,
                    },
                    RelocationEncoding::Generic,
                    32,
                )
            }
            Reloc::Aarch64TlsGdAdrPage21 => {
                format_assert!(Elf, Aarch64TlsGdAdrPage21);
                (
                    RelocationKind::Elf(object::elf::R_AARCH64_TLSGD_ADR_PAGE21),
                    RelocationEncoding::Generic,
                    21,
                )
            }
            Reloc::Aarch64TlsGdAddLo12Nc => {
                format_assert!(Elf, Aarch64TlsGdAddLo12Nc);
                (
                    RelocationKind::Elf(object::elf::R_AARCH64_TLSGD_ADD_LO12_NC),
                    RelocationEncoding::Generic,
                    12,
                )
            }
            Reloc::S390xPCRel32Dbl => (RelocationKind::Relative, RelocationEncoding::S390xDbl, 32),
            Reloc::S390xPLTRel32Dbl => (
                RelocationKind::PltRelative,
                RelocationEncoding::S390xDbl,
                32,
            ),
            Reloc::S390xTlsGd64 => {
                format_assert!(Elf, S390xTlsGd64);
                (
                    RelocationKind::Elf(object::elf::R_390_TLS_GD64),
                    RelocationEncoding::Generic,
                    64,
                )
            }
            Reloc::S390xTlsGdCall => {
                format_assert!(Elf, S390xTlsGdCall);
                (
                    RelocationKind::Elf(object::elf::R_390_TLS_GDCALL),
                    RelocationEncoding::Generic,
                    0,
                )
            }
            // FIXME
            reloc => unimplemented!("{:?}", reloc),
        };

        let symbol = match record.name {
            crate::GenItemName::Func(func) => {
                self.functions[func]
                    .expect("Function should be loaded at this point.")
                    .symbol
            }
        };

        Relocation {
            offset: record.offset as u64 + symbol_offset,
            size,
            kind,
            encoding,
            symbol,
            addend,
        }
    }

    pub fn emit(&mut self) -> object::write::Result<Vec<u8>> {
        self.object.write()
    }

    fn translate_visibility(visibility: FuncVisibility) -> (SymbolScope, bool) {
        let scope = match visibility {
            FuncVisibility::Local => SymbolScope::Compilation,
            FuncVisibility::Exported => SymbolScope::Dynamic,
            FuncVisibility::Imported => SymbolScope::Unknown,
        };

        (scope, false)
    }
}

#[derive(Debug)]
pub enum ObjectCreationError {
    UnsupportedBinaryFormat(target_lexicon::BinaryFormat),
    UnsupportedArchitecture(target_lexicon::Architecture),
    MissingEndianness,
}

impl Display for ObjectCreationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedBinaryFormat(format) => {
                write!(f, "unsupported binary format: {:?}", format)
            }
            Self::UnsupportedArchitecture(architecture) => {
                write!(f, "unsupported architecture: {:?}", architecture)
            }
            Self::MissingEndianness => write!(f, "missing endianness"),
        }
    }
}

impl std::error::Error for ObjectCreationError {}
