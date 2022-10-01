use std::fmt::Display;

use ::object::{RelocationEncoding, RelocationKind, SymbolScope};
use cranelift_codegen::binemit::Reloc;
use object::{
    write::{Object, Relocation, SectionId, StandardSection, Symbol, SymbolId, SymbolSection},
    Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind,
};
use storage::*;
use typec_t::*;

use crate::{context::Isa, *};

#[derive(Clone, Copy)]
struct ObjectFunction {
    symbol: SymbolId,
}

pub struct ObjectContext {
    functions: ShadowMap<CompiledFunc, Option<ObjectFunction>>,
    text_section: SectionId,
    object: Object<'static>,
    pub isa: Isa,
}

impl ObjectContext {
    pub fn new(isa: Isa) -> Result<Self, ObjectCreationError> {
        let triple = isa.triple();
        let binary_format = match triple.binary_format {
            target_lexicon::BinaryFormat::Elf => BinaryFormat::Elf,
            target_lexicon::BinaryFormat::Coff => BinaryFormat::Coff,
            target_lexicon::BinaryFormat::Macho => BinaryFormat::MachO,
            target_lexicon::BinaryFormat::Wasm => BinaryFormat::Wasm,
            other => Err(ObjectCreationError::UnsupportedBinaryFormat(other))?,
        };

        let architecture = match triple.architecture {
            target_lexicon::Architecture::X86_32(_) => Architecture::I386,
            target_lexicon::Architecture::X86_64 => Architecture::X86_64,
            target_lexicon::Architecture::Arm(_) => Architecture::Arm,
            target_lexicon::Architecture::Aarch64(_) => Architecture::Aarch64,
            target_lexicon::Architecture::S390x => Architecture::S390x,
            other => Err(ObjectCreationError::UnsupportedArchitecture(other))?,
        };

        let endian = match triple
            .endianness()
            .map_err(|()| ObjectCreationError::MissingEndianness)?
        {
            target_lexicon::Endianness::Little => Endianness::Little,
            target_lexicon::Endianness::Big => Endianness::Big,
        };

        let mut object = Object::new(binary_format, architecture, endian);
        Ok(Self {
            functions: ShadowMap::new(),
            text_section: object.section_id(StandardSection::Text),
            object,
            isa,
        })
    }

    pub fn load_functions(
        &mut self,
        funcs: impl IntoIterator<Item = VRef<CompiledFunc>>,
        gen: &Gen,
        typec: &Typec,
        interner: &Interner,
    ) -> Result<(), ObjectRelocationError> {
        // register functions
        let mut funcs = funcs
            .into_iter()
            .filter_map(|func| self.functions[func].is_none().then_some(func))
            .map(|func| {
                let ent = &gen.compiled_funcs[func];

                let id = gen.compiled_funcs.id(func);

                let meta = &typec.funcs[ent.func];
                let (scope, weak) = Self::translate_visibility(meta.visibility);

                let name = if scope == SymbolScope::Unknown {
                    &interner[meta.loc.name]
                } else {
                    &interner[id]
                };

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

                (func, symbol, 0, scope == SymbolScope::Unknown) // preallocate space for offset
            })
            .collect::<BumpVec<_>>();

        // filter out extern functions and insert symbols
        funcs.retain(|&(func, symbol, _, is_extern)| {
            self.functions[func] = Some(ObjectFunction { symbol });
            !is_extern
        });

        // add function bodies and fill offsets
        for &mut (func, symbol, ref mut offset, ..) in funcs.iter_mut() {
            let ent = &gen.compiled_funcs[func];

            *offset = self.object.add_symbol_data(
                symbol,
                self.text_section,
                &ent.bytecode,
                ent.alignment,
            );
        }

        // add relocations
        for (func, _, offset, ..) in funcs {
            let ent = &gen.compiled_funcs[func];
            for &record in &ent.relocs {
                let reloc = self.process_reloc(record, offset)?;
                self.object
                    .add_relocation(self.text_section, reloc)
                    .map_err(ObjectRelocationError::AddRelocation)?;
            }
        }

        Ok(())
    }

    fn process_reloc(
        &self,
        record: GenReloc,
        symbol_offset: u64,
    ) -> Result<Relocation, ObjectRelocationError> {
        let assert_format = |expected, variant| {
            if expected != self.object.format() {
                Err(ObjectRelocationError::Unsupported(
                    variant,
                    expected,
                    self.object.format(),
                ))?
            } else {
                Ok(())
            }
        };

        let mut addend = record.addend as i64;
        use Reloc::*;
        use RelocationEncoding::*;
        use RelocationKind::*;
        let (kind, encoding, size) = match record.kind {
            Abs4 => (Absolute, Generic, 32),
            Abs8 => (Absolute, Generic, 64),
            X86PCRel4 => (Relative, Generic, 32),
            X86CallPCRel4 => (Relative, X86Branch, 32),
            X86CallPLTRel4 => (PltRelative, X86Branch, 32),
            X86SecRel => (SectionOffset, Generic, 32),
            X86GOTPCRel4 => (GotRelative, Generic, 32),
            Arm64Call => (Relative, AArch64Call, 26),
            S390xPCRel32Dbl => (Relative, S390xDbl, 32),
            S390xPLTRel32Dbl => (PltRelative, S390xDbl, 32),
            ElfX86_64TlsGd => {
                assert_format(BinaryFormat::Elf, ElfX86_64TlsGd)?;
                (Elf(object::elf::R_X86_64_TLSGD), Generic, 32)
            }
            MachOX86_64Tlv => {
                assert_format(BinaryFormat::MachO, MachOX86_64Tlv)?;
                addend += 4; // X86_64_RELOC_TLV has an implicit addend of -4
                (
                    MachO {
                        value: object::macho::X86_64_RELOC_TLV,
                        relative: true,
                    },
                    Generic,
                    32,
                )
            }
            Aarch64TlsGdAdrPage21 => {
                assert_format(BinaryFormat::Elf, Aarch64TlsGdAdrPage21)?;
                (Elf(object::elf::R_AARCH64_TLSGD_ADR_PAGE21), Generic, 21)
            }
            Aarch64TlsGdAddLo12Nc => {
                assert_format(BinaryFormat::Elf, Aarch64TlsGdAddLo12Nc)?;
                (Elf(object::elf::R_AARCH64_TLSGD_ADD_LO12_NC), Generic, 12)
            }
            S390xTlsGd64 => {
                assert_format(BinaryFormat::Elf, S390xTlsGd64)?;
                (Elf(object::elf::R_390_TLS_GD64), Generic, 64)
            }
            S390xTlsGdCall => {
                assert_format(BinaryFormat::Elf, S390xTlsGdCall)?;
                (Elf(object::elf::R_390_TLS_GDCALL), Generic, 0)
            }
            // FIXME
            reloc => unimplemented!("{:?}", reloc),
        };

        let symbol = match record.name {
            crate::GenItemName::Func(func) => {
                self.functions[func]
                    .ok_or(ObjectRelocationError::MissingSymbol(func))?
                    .symbol
            }
        };

        Ok(Relocation {
            offset: record.offset as u64 + symbol_offset,
            size,
            kind,
            encoding,
            symbol,
            addend,
        })
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
pub enum ObjectRelocationError {
    Unsupported(Reloc, BinaryFormat, BinaryFormat),
    MissingSymbol(VRef<CompiledFunc>),
    AddRelocation(object::write::Error),
}

impl Display for ObjectRelocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectRelocationError::Unsupported(reloc, format, correct) => write!(
                f,
                "relocation {:?} is not supported for the {:?} format, only on {:?} format",
                reloc, format, correct
            ),
            ObjectRelocationError::MissingSymbol(func) => {
                write!(f, "missing symbol for function {:?}", func)
            }
            ObjectRelocationError::AddRelocation(err) => {
                write!(f, "failed to add relocation: {}", err)
            }
        }
    }
}

impl std::error::Error for ObjectRelocationError {}

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
