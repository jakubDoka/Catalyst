use module_types::*;
use storage::*;
use lexer_types::*;

use crate::*;

pub type ToCompile = Vec<FuncRef>;
pub type Funcs = PrimaryMap<Func, TFuncEnt>;

#[derive(Clone, Copy)]
pub enum FuncRef {
    Func(Func),
    ID(ID, PackedOption<Func>),
}

#[derive(Debug, Copy, Clone, Default)]
pub struct TFuncEnt {
    pub sig: Sig,
    pub id: ID,
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

    pub fn home_module_id(&self, ty_lists: &TyLists, modules: &Modules, types: &Types) -> ID {
        let home_module = self.home_module(ty_lists, modules, types);
        modules[home_module].id
    }

    pub fn home_module(&self, ty_lists: &TyLists, modules: &Modules, types: &Types) -> Source {
        let def_loc = self.name.source();
        
        ty_lists.get(self.sig.params).iter().fold(def_loc, |acc, &ty| {
            let other = types[ty].name.source();
            if modules[other].ordering > modules[acc].ordering {
                other
            } else {
                acc
            }
        })
    }

    // pub fn get_link_name(&self, types: &Types, ty_lists: &TyLists, sources: &Sources, buffer: &mut String) {
    //     buffer.write_str(sources.display(self.name)).unwrap();
    //     if !self.sig.params.is_reserved_value() {
    //         buffer.write_char('[').unwrap();
    //         for &ty in ty_lists.get(self.sig.params) {
    //             ty.display(types, ty_lists, sources, buffer).unwrap();
    //             buffer.write_char(',').unwrap();
    //         }
    //         buffer.pop().unwrap();
    //         buffer.write_char(']').unwrap();
    //     }
    // }
}

bitflags! {
    #[derive(Default)]
    pub struct TFuncFlags: u32 {
        const ENTRY = 1 << 0;
        const GENERIC = 1 << 1;
        const INLINE = 1 << 2;
        const EXTERNAL = 1 << 3;
        const STRUCT_RET = 1 << 4;
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
    pub ty_lists: &'a TyLists,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(sources: &'a Sources, ty_lists: &'a TyLists, types: &'a Types, sig: &'a Sig) -> Self {
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
        write!(
            f,
            ") -> {}",
            ty_display!(self, self.sig.ret),
        )?;
        Ok(())
    }
}

gen_entity!(Func);
gen_entity!(FuncList);