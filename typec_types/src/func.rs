use storage::*;
use lexer_types::*;

use crate::*;

use std::fmt::Write;

pub type Funcs = PrimaryMap<Func, TFuncEnt>;

#[derive(Debug, Copy, Clone, Default)]
pub struct TFuncEnt {
    pub sig: Sig,
    pub name: Span,
    pub kind: TFuncKind,
    pub body: Tir,
    pub args: TirList,
    pub flags: TFuncFlags,
}

impl TFuncEnt {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_link_name(&self, types: &Types, sources: &Sources, buffer: &mut String) {
        buffer.write_str(sources.display(self.name)).unwrap();
        if !self.sig.params.is_reserved_value() {
            buffer.write_char('[').unwrap();
            for &ty in types.args.get(self.sig.params) {
                ty.display(types, sources, buffer).unwrap();
                buffer.write_char(',').unwrap();
            }
            buffer.pop().unwrap();
            buffer.write_char(']').unwrap();
        }
    }
}

bitflags! {
    #[derive(Default)]
    pub struct TFuncFlags: u32 {
        const ENTRY = 1 << 0;
        const GENERIC = 1 << 1;
        const INLINE = 1 << 2;
        const EXTERNAL = 1 << 3;
    }
}

impl_bool_bit_and!(TFuncFlags);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TFuncKind {
    Local,
    External,
    // (bound, relative index)
    Bound(Ty, u32),
    Owned(Ty),
    Instance(Func),
    Builtin,
}

impl Default for TFuncKind {
    fn default() -> Self {
        TFuncKind::Local
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Sig {
    pub params: TyList,
    pub call_conv: Span,
    pub args: TyList,
    pub ret: Ty,
}

pub struct SignatureDisplay<'a> {
    pub sig: &'a Sig,
    pub sources: &'a Sources,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(sig: &'a Sig, sources: &'a Sources, types: &'a Types) -> Self {
        SignatureDisplay {
            sig,
            types,
            sources,
        }
    }
}

impl std::fmt::Display for SignatureDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, &ty) in self.types.args.get(self.sig.args).iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", TyDisplay::new(self.types, self.sources, ty))?;
        }
        write!(
            f,
            ") -> {}",
            TyDisplay::new(self.types, self.sources, self.sig.ret)
        )?;
        Ok(())
    }
}

gen_entity!(Func);
gen_entity!(FuncList);