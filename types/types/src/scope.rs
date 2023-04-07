use std::collections::hash_map;

use diags::SourceLoc;
use resources::{Resources, Source};
use rkyv::{Archive, Deserialize, Serialize};
use span::*;
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

    pub fn start_frame(&self) -> ScopeFrame {
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
        current: VRef<Source>,
        foreign: VRef<Source>,
        item: ModuleItem,
        resources: &Resources,
        interner: &mut Interner,
    ) {
        debug_assert!(current != foreign);

        let (position, accessible) =
            Self::compute_accessibility(current, foreign, item.vis, resources);

        let record = ScopeRecord::Imported {
            source: foreign,
            item,
            position,
            accessible,
        };

        let scoped_id = interner.intern_scoped(foreign.index(), item.id);
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
        current: VRef<Source>,
        foreign: VRef<Source>,
        vis: Option<Vis>,
        resources: &Resources,
    ) -> (Option<ScopePosition>, bool) {
        if current == foreign {
            return (None, true);
        }

        if resources.sources[current].package == resources.sources[foreign].package {
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

wrapper_enum! {
    #[derive(Clone, Copy, PartialEq, Eq, Debug, Deserialize, Serialize, Archive)]
    enum ModuleItemPtr: relocated {
        Func: FragRef<Func>,
        Ty: Ty,
        SpecBase: FragRef<SpecBase>,
        Impl: FragRef<Impl>,
        Const: FragRef<Const>,
    }
}

wrapper_enum! {
    #[derive(Clone, Copy, PartialEq, Eq, Debug, Deserialize, Serialize, Archive)]
    enum ScopeItem: {
        Func: FragRef<Func> => "function",
        Ty: Ty => "type",
        SpecFunc: FragRef<SpecFunc> => "spec function",
        SpecBase: FragRef<SpecBase> => "spec",
        VarHeaderTir: VRef<VarHeaderTir> => "variable",
        Module: VRef<Source> => "module",
        LoopHeaderTir: VRef<LoopHeaderTir> => "loop label",
        Const: FragRef<Const> => "constant",
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
