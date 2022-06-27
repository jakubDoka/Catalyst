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

        ty_lists.get(self.params).iter().fold(def_loc, |acc, &ty| {
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
        const NO_OWNERSHIP = 1 << 6;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Sig {
    pub cc: Option<CallConv>,
    pub args: TyCompList,
    pub ret: Ty,
}

impl Default for Sig {
    fn default() -> Self {
        Self {
            cc: Some(CallConv::Fast),
            args: TyCompList::default(),
            ret: Ty::default(),
        }
    }
}

impl SigDisplay<'_> {
    pub fn write(&self, f: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        write!(f, "fn (")?;
        for (i, &arg) in self.ty_comps.get(self.sig.args).iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            ty_display!(self, arg.ty).write(f)?;
        }
        ty_display!(self, self.sig.ret).write(f)?;
        Ok(())
    }
}

impl std::fmt::Display for SigDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        self.write(&mut s)?;
        write!(f, "{s}")
    }
}

gen_entity!(Func);
gen_entity!(FuncList);
