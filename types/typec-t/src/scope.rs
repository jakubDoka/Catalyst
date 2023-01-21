use std::collections::hash_map;

use bytecheck::CheckBytes;
use diags::SourceLoc;
use lexing_t::*;
use packaging_t::{Module, Resources, Source};
use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

use crate::*;

#[derive(Default)]
pub struct Scope {
    data: Map<Ident, ScopeRecord>,
    pushed: Vec<(Ident, Option<ScopeRecord>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, ident: Ident) -> Result<ScopeItem, ScopeError> {
        self.data
            .get(&ident)
            .ok_or(ScopeError::NotFound)
            .and_then(|record| record.accessible())
            .and_then(|option| option.scope_item().ok_or(ScopeError::Collision))
    }

    pub fn push(&mut self, id: Ident, item: impl Into<ScopeItem>, span: Span) {
        let record = ScopeRecord::Pushed {
            kind: item.into(),
            span,
        };
        self.pushed.push((id, self.data.insert(id, record)));
    }

    pub fn start_frame(&mut self) -> ScopeFrame {
        ScopeFrame(self.pushed.len())
    }

    pub fn end_frame(&mut self, frame: ScopeFrame) {
        for (id, item) in self.pushed.drain(frame.0..) {
            if let Some(item) = item {
                self.data.insert(id, item);
            } else {
                self.data.remove(&id);
            }
        }
    }

    pub fn insert_builtin(&mut self, id: Ident, item: impl Into<ScopeItem>) {
        self.data
            .insert(id, ScopeRecord::Builtin { kind: item.into() });
    }

    pub fn insert_current(&mut self, item: ModuleItem) -> Result<(), ScopeRecord> {
        match self.data.entry(item.id) {
            hash_map::Entry::Occupied(e) => Err(*e.get()),
            hash_map::Entry::Vacant(e) => {
                e.insert(ScopeRecord::Current { item });
                Ok(())
            }
        }
    }

    pub fn insert(
        &mut self,
        current_module: VRef<Module>,
        foreign_module: VRef<Module>,
        item: ModuleItem,
        resources: &Resources,
        interner: &mut Interner,
    ) {
        debug_assert!(current_module != foreign_module);

        let (position, accessible) =
            Self::compute_accessibility(current_module, foreign_module, item.vis, resources);

        let record = ScopeRecord::Imported {
            module: foreign_module,
            source: resources.modules[foreign_module].source,
            item,
            position,
            accessible,
        };

        let scoped_id = interner.intern_scoped(foreign_module.index(), item.id);
        self.data.insert(scoped_id, record);

        if let Some(existing_option) = self.data.get_mut(&item.id) {
            match existing_option {
                ScopeRecord::Imported { .. } => {
                    *existing_option = ScopeRecord::Collision;
                }
                ScopeRecord::Current { .. }
                | ScopeRecord::Builtin { .. }
                | ScopeRecord::Pushed { .. }
                | ScopeRecord::Collision => (),
            }
        } else {
            self.data.insert(item.id, record);
        }
    }

    pub fn compute_accessibility(
        current_module: VRef<Module>,
        foreign_module: VRef<Module>,
        vis: Option<Vis>,
        resources: &Resources,
    ) -> (Option<ScopePosition>, bool) {
        if current_module == foreign_module {
            return (None, true);
        }

        if resources.modules[current_module].package == resources.modules[foreign_module].package {
            return (Some(ScopePosition::Module), vis != Some(Vis::Priv));
        }

        (Some(ScopePosition::Package), vis == Some(Vis::Pub))
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.pushed.clear();
    }
}

#[must_use]
pub struct ScopeFrame(usize);

pub enum ScopeError {
    NotFound,
    Collision,
    Inaccessible(ScopePosition, SourceLoc),
}

#[derive(Clone, Copy, Debug)]
pub enum ScopePosition {
    Module,
    Package,
}

#[derive(Clone, Copy, Debug)]
pub enum ScopeRecord {
    Imported {
        module: VRef<Module>,
        source: VRef<Source>,
        item: ModuleItem,
        position: Option<ScopePosition>,
        accessible: bool,
    },
    Current {
        item: ModuleItem,
    },
    Builtin {
        kind: ScopeItem,
    },
    Pushed {
        kind: ScopeItem,
        span: Span,
    },
    Collision,
}

impl ScopeRecord {
    pub fn scope_item(&self) -> Option<ScopeItem> {
        match *self {
            Self::Imported { item, .. } | Self::Current { item } => item.ptr.into(),
            Self::Builtin { kind } | Self::Pushed { kind, .. } => Some(kind),
            Self::Collision => None,
        }
    }

    pub fn span(&self) -> Option<Span> {
        match *self {
            Self::Imported { item, .. } | Self::Current { item } => Some(item.span),
            Self::Builtin { .. } => None,
            Self::Pushed { span, .. } => Some(span),
            Self::Collision => None,
        }
    }

    pub fn is_strong(&self) -> bool {
        match *self {
            Self::Collision | Self::Imported { .. } => false,
            Self::Builtin { .. } | Self::Pushed { .. } | Self::Current { .. } => true,
        }
    }

    fn accessible(&self) -> Result<Self, ScopeError> {
        match *self {
            Self::Imported {
                accessible: false,
                position: Some(pos),
                item,
                source,
                ..
            } => Err(ScopeError::Inaccessible(
                pos,
                SourceLoc {
                    origin: source,
                    span: item.span,
                },
            )),
            s => Ok(s),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize, Deserialize, Archive)]
#[archive_attr(derive(CheckBytes))]
pub struct ModuleItem {
    pub id: Ident,
    pub ptr: ModuleItemPtr,
    pub span: Span,
    pub vis: Option<Vis>,
}

derive_relocated!(struct ModuleItem { ptr });

impl ModuleItem {
    pub fn new(id: Ident, ptr: impl Into<ModuleItemPtr>, span: Span, vis: Option<Vis>) -> Self {
        Self {
            id,
            ptr: ptr.into(),
            span,
            vis,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Deserialize, Serialize, Archive)]
#[archive_attr(derive(CheckBytes))]
pub enum ModuleItemPtr {
    Func(FragRef<Func>),
    Ty(Ty),
    SpecBase(FragRef<SpecBase>),
    Impl(FragRef<Impl>),
    Const(FragRef<Const>),
}

derive_relocated!(enum ModuleItemPtr {
    Func(f) => f,
    Ty(t) => t,
    SpecBase(b) => b,
    Impl(i) => i,
    Const(c) => c,
});

impl From<FragRef<Func>> for ModuleItemPtr {
    fn from(func: FragRef<Func>) -> Self {
        Self::Func(func)
    }
}

impl From<Ty> for ModuleItemPtr {
    fn from(ty: Ty) -> Self {
        Self::Ty(ty)
    }
}

impl From<FragRef<SpecBase>> for ModuleItemPtr {
    fn from(base: FragRef<SpecBase>) -> Self {
        Self::SpecBase(base)
    }
}

impl From<FragRef<Impl>> for ModuleItemPtr {
    fn from(r#impl: FragRef<Impl>) -> Self {
        Self::Impl(r#impl)
    }
}

impl From<FragRef<Const>> for ModuleItemPtr {
    fn from(r#const: FragRef<Const>) -> Self {
        Self::Const(r#const)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeItem {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>),
    Ty(Ty),
    SpecBase(FragRef<SpecBase>),
    VarHeaderTir(VRef<VarHeaderTir>),
    Module(VRef<Module>),
    LoopHeaderTir(VRef<LoopHeaderTir>),
    Const(FragRef<Const>),
}

macro_rules! gen_scope_item {
    ($($ref:ident $name:ident),*) => {
        $(
            impl From<$ref<$name>> for ScopeItem {
                fn from(item: $ref<$name>) -> Self {
                    Self::$name(item)
                }
            }
        )*
    }
}

impl ScopeItem {
    pub fn name(&self) -> &'static str {
        match *self {
            ScopeItem::Func(..) => "function",
            ScopeItem::SpecFunc(..) => "spec function",
            ScopeItem::Ty(..) => "type",
            ScopeItem::SpecBase(..) => "spec",
            ScopeItem::VarHeaderTir(..) => "variable",
            ScopeItem::Module(..) => "module",
            ScopeItem::LoopHeaderTir(..) => "loop label",
            ScopeItem::Const(..) => "constant",
        }
    }
}

gen_scope_item!(FragRef SpecFunc, FragRef Func, VRef VarHeaderTir, VRef Module, FragRef SpecBase, VRef LoopHeaderTir, FragRef Const);

impl From<Ty> for ScopeItem {
    fn from(item: Ty) -> Self {
        Self::Ty(item)
    }
}

impl From<ModuleItemPtr> for Option<ScopeItem> {
    fn from(item: ModuleItemPtr) -> Self {
        Some(match item {
            ModuleItemPtr::Func(func) => func.into(),
            ModuleItemPtr::Ty(ty) => ty.into(),
            ModuleItemPtr::SpecBase(base) => base.into(),
            ModuleItemPtr::Const(c) => c.into(),
            ModuleItemPtr::Impl(..) => return None,
        })
    }
}
