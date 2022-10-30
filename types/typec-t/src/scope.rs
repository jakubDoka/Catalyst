use lexing_t::*;
use packaging_t::Module;
use parsing_t::*;

use storage::*;

use crate::*;

#[derive(Default)]
pub struct Scope {
    data: Map<FragSlice<u8>, ScopeRecord>,
    pushed: Vec<(FragSlice<u8>, Option<ScopeRecord>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, ident: FragSlice<u8>) -> Result<ScopeItem, ScopeError> {
        self.data
            .get(&ident)
            .map(|option| option.scope_item().ok_or(ScopeError::Collision))
            .ok_or(ScopeError::NotFound)
            .flatten()
    }

    pub fn push(&mut self, id: FragSlice<u8>, item: impl Into<ScopeItem>, span: Span) {
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

    pub fn insert_builtin(&mut self, id: FragSlice<u8>, item: impl Into<ScopeItem>) {
        self.data
            .insert(id, ScopeRecord::Builtin { kind: item.into() });
    }

    pub fn insert_current(&mut self, item: ModuleItem) -> Result<(), Option<Span>> {
        self.data
            .insert(item.id, ScopeRecord::CurrentItem { item })
            .filter(|record| record.is_strong())
            .map(|record| Result::<(), _>::Err(record.span()))
            .transpose()
            .map(|_| ())
    }

    pub fn insert(
        &mut self,
        current_module: VRef<Module>,
        foreign_module: VRef<Module>,
        item: ModuleItem,
        interner: &mut Interner,
    ) {
        debug_assert!(current_module != foreign_module);

        let scoped_id = interner.intern_scoped(foreign_module.index(), item.id);
        self.data.insert(
            scoped_id,
            ScopeRecord::ImportedItem {
                module: foreign_module,
                item,
            },
        );

        if let Some(existing_option) = self.data.get_mut(&item.id) {
            match existing_option {
                ScopeRecord::ImportedItem { .. } => {
                    *existing_option = ScopeRecord::Collision;
                }
                ScopeRecord::CurrentItem { .. }
                | ScopeRecord::Builtin { .. }
                | ScopeRecord::Pushed { .. }
                | ScopeRecord::Collision => (),
            }
        } else {
            self.data.insert(
                item.id,
                ScopeRecord::ImportedItem {
                    module: foreign_module,
                    item,
                },
            );
        }
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
}

#[derive(Clone, Copy)]
pub enum ScopeRecord {
    ImportedItem {
        module: VRef<Module>,
        item: ModuleItem,
    },
    CurrentItem {
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
            Self::ImportedItem { item, .. } | Self::CurrentItem { item } => Some(item.ptr.into()),
            Self::Builtin { kind } | Self::Pushed { kind, .. } => Some(kind),
            Self::Collision => None,
        }
    }

    pub fn span(&self) -> Option<Span> {
        match *self {
            Self::ImportedItem { item, .. } | Self::CurrentItem { item } => Some(item.span),
            Self::Builtin { .. } => None,
            Self::Pushed { span, .. } => Some(span),
            Self::Collision => None,
        }
    }

    pub fn is_strong(&self) -> bool {
        match *self {
            Self::Collision | Self::ImportedItem { .. } => false,
            Self::Builtin { .. } | Self::Pushed { .. } | Self::CurrentItem { .. } => true,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ModuleItem {
    pub id: FragSlice<u8>,
    pub ptr: ModuleItemPtr,
    pub span: Span,
    pub vis: Vis,
}

impl ModuleItem {
    pub fn new(id: FragSlice<u8>, ptr: impl Into<ModuleItemPtr>, span: Span, vis: Vis) -> Self {
        Self {
            id,
            ptr: ptr.into(),
            span,
            vis,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ModuleItemPtr {
    Func(FragRef<Func>),
    Ty(Ty),
    SpecBase(FragRef<SpecBase>),
}

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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScopeItem {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>),
    Ty(Ty),
    SpecBase(FragRef<SpecBase>),
    VarHeaderTir(VRef<VarHeaderTir>),
    Module(VRef<Module>),
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

        impl ScopeItem {
           pub fn what(&self) -> &'static str {
                match *self {
                    $(Self::$name(..) => stringify!($name),)*
                    Self::Ty(..) => "parameter",
                }
            }
        }
    }
}

gen_scope_item!(FragRef SpecFunc, FragRef Func, VRef VarHeaderTir, VRef Module, FragRef SpecBase);

impl From<Ty> for ScopeItem {
    fn from(item: Ty) -> Self {
        Self::Ty(item)
    }
}

impl From<ModuleItemPtr> for ScopeItem {
    fn from(item: ModuleItemPtr) -> Self {
        match item {
            ModuleItemPtr::Func(func) => func.into(),
            ModuleItemPtr::Ty(ty) => ty.into(),
            ModuleItemPtr::SpecBase(base) => base.into(),
        }
    }
}
