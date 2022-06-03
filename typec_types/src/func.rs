use cranelift_codegen::isa::CallConv;
use lexer::*;
use module_types::*;
use storage::*;

use crate::{jit::Macro, *};

pub type Initializers = Vec<(Func, PackedOption<Global>)>;
pub type ToCompile = Vec<(Func, TyList)>;
pub type Funcs = storage::MetaMap<Func, FuncEnt, FuncMeta>;
pub type FuncInstances = Map<Func>;
pub type ToLink = Vec<Func>;
pub type Macros = Vec<(Func, Macro)>;

impl HasMeta for Func {}

#[derive(Clone, Copy, Default)]
pub struct FuncEnt {
    pub id: ID,
    pub flags: FuncFlags,
}

#[derive(Clone, Default)]
pub struct FuncMeta {
    pub params: TyList,
    pub sig: Sig,
    pub name: Span,
    pub kind: FuncKind,
    pub body: Tir,
    pub args: TirList,
    pub tir_data: TirData,
}

impl FuncMeta {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn home_module_id(&self, ty_lists: &TyLists, modules: &Modules, types: &Types) -> ID {
        let home_module = self.home_module(ty_lists, modules, types);
        modules[home_module].id
    }

    pub fn home_module(&self, ty_lists: &TyLists, modules: &Modules, types: &Types) -> Source {
        let def_loc = self.name.source();

        ty_lists
            .get(self.params)
            .iter()
            .fold(def_loc, |acc, &ty| {
                let other = types[ty].name.source();
                if modules[other].ordering > modules[acc].ordering {
                    other
                } else {
                    acc
                }
            })
    }
}

bitflags! {
    #[derive(Default)]
    pub struct FuncFlags: u32 {
        const ENTRY = 1 << 0;
        const GENERIC = 1 << 1;
        const INLINE = 1 << 2;
        const EXTERNAL = 1 << 3;
        const STRUCT_RET = 1 << 4;
        const ANONYMOUS = 1 << 5;
    }
}

impl FuncFlags {
    const CALL_CONV_OFFSET: u32 = 32 - 8;
    const CALL_CONV_MASK: u32 = 0xFF << Self::CALL_CONV_OFFSET;

    pub fn set_call_conv(&mut self, cc: Option<CallConv>) {
        let Some(cc) = cc else {
            self.bits &= !Self::CALL_CONV_MASK;
            self.bits &= !(1 << (Self::CALL_CONV_OFFSET - 1));
            return;
        };

        let bytes = unsafe { std::mem::transmute::<_, u8>(cc) } as u32;

        self.bits &= !Self::CALL_CONV_MASK;
        self.bits |= bytes << Self::CALL_CONV_OFFSET;
        self.bits |= 1 << (Self::CALL_CONV_OFFSET - 1);
    }

    pub fn call_conv(&self) -> Option<CallConv> {
        if self.bits & (1 << (Self::CALL_CONV_OFFSET - 1)) == 0 {
            return None;
        }

        let bytes = (self.bits & Self::CALL_CONV_MASK) >> Self::CALL_CONV_OFFSET;
        unsafe { Some(std::mem::transmute::<_, CallConv>(bytes as u8)) }
    }
}

impl std::ops::BitOr<Option<CallConv>> for FuncFlags {
    type Output = Self;

    fn bitor(self, rhs: Option<CallConv>) -> Self {
        let mut result = self;
        result.set_call_conv(rhs);
        result
    }
}

impl_bool_bit_and!(FuncFlags);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FuncKind {
    Local,
    External,
    // (bound, relative index)
    Bound(Ty, u32),
    Owned(Ty),
    Builtin,
}

impl Default for FuncKind {
    fn default() -> Self {
        FuncKind::Local
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Sig {
    pub args: TyList,
    pub ret: Ty,
}

pub struct SignatureDisplay<'a> {
    pub sig: &'a Sig,
    pub sources: &'a Sources,
    pub ty_lists: &'a TyLists,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(
        sources: &'a Sources,
        ty_lists: &'a TyLists,
        types: &'a Types,
        sig: &'a Sig,
    ) -> Self {
        SignatureDisplay {
            sig,
            types,
            ty_lists,
            sources,
        }
    }
}

impl std::fmt::Display for SignatureDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, &ty) in self.ty_lists.get(self.sig.args).iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty_display!(self, ty))?;
        }
        write!(f, ") -> {}", ty_display!(self, self.sig.ret),)?;
        Ok(())
    }
}

gen_entity!(Func);
gen_entity!(FuncList);
