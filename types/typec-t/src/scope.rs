use std::fmt;

use lexing_t::*;
use parsing_t::*;
use storage::*;

use crate::*;

#[derive(Default)]
pub struct Scope {
    data: Map<VRef<str>, ScopeRecord>,
    pushed: Vec<(VRef<str>, Option<ScopeRecord>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, ident: VRef<str>) -> Result<ScopeItem, ScopeError> {
        self.data
            .get(&ident)
            .map(|option| option.scope_item().ok_or(ScopeError::Collision))
            .ok_or(ScopeError::NotFound)
            .flatten()
    }

    pub fn push(&mut self, id: VRef<str>, item: impl Into<ScopeItem>, span: Span) {
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

    pub fn insert_builtin(&mut self, id: VRef<str>, item: impl Into<ScopeItem>) {
        self.data
            .insert(id, ScopeRecord::Builtin { kind: item.into() });
    }

    pub fn insert_current(&mut self, item: ModItem) -> Result<(), Option<(Span, Span)>> {
        self.data
            .insert(item.id, ScopeRecord::CurrentItem { item })
            .filter(|record| record.is_strong())
            .map(|record| Result::<(), _>::Err(record.spans()))
            .transpose()
            .map(|_| ())
    }

    pub fn insert(
        &mut self,
        current_module: VRef<str>,
        foreign_module: VRef<str>,
        item: ModItem,
    ) -> Result<(), Option<(Span, Span)>> {
        debug_assert!(current_module != foreign_module);

        if let Some(existing_option) = self.data.get_mut(&item.id) {
            match existing_option {
                ScopeRecord::ImportedItem { .. } => {
                    *existing_option = ScopeRecord::Collision;
                    Ok(())
                }
                ScopeRecord::CurrentItem { .. }
                | ScopeRecord::Builtin { .. }
                | ScopeRecord::Pushed { .. } => Err(existing_option.spans()),
                ScopeRecord::Collision => Ok(()),
            }
        } else {
            self.data.insert(
                item.id,
                ScopeRecord::ImportedItem {
                    module: foreign_module,
                    item,
                },
            );
            Ok(())
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.pushed.clear();
    }
}

pub struct ScopeFrame(usize);

pub enum ScopeError {
    NotFound,
    Collision,
}

#[derive(Clone, Copy)]
pub enum ScopeRecord {
    ImportedItem { module: VRef<str>, item: ModItem },
    CurrentItem { item: ModItem },
    Builtin { kind: ScopeItem },
    Pushed { kind: ScopeItem, span: Span },
    Collision,
}

impl ScopeRecord {
    pub fn scope_item(&self) -> Option<ScopeItem> {
        match *self {
            Self::ImportedItem { item, .. } | Self::CurrentItem { item } => Some(item.ptr),
            Self::Builtin { kind } | Self::Pushed { kind, .. } => Some(kind),
            Self::Collision => None,
        }
    }

    pub fn spans(&self) -> Option<(Span, Span)> {
        match *self {
            Self::ImportedItem { item, .. } | Self::CurrentItem { item } => {
                Some((item.span, item.whole_span))
            }
            Self::Builtin { .. } => None,
            Self::Pushed { span, .. } => Some((span, span)),
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

#[derive(Clone, Copy)]
pub struct ModItem {
    pub id: VRef<str>,
    pub ptr: ScopeItem,
    pub span: Span,
    pub whole_span: Span,
    pub vis: Vis,
}

#[derive(Clone, Copy)]
pub enum ScopeItem {
    Func(VRef<Func>),
    Ty(VRef<Ty>),
    Spec(VRef<Spec>),
    Var(VRef<Var>),
    Mod(VRef<str>),
}

macro_rules! gen_scope_item {
    ($($name:ident),*) => {
        $(
            impl From<VRef<$name>> for ScopeItem {
                fn from(item: VRef<$name>) -> Self {
                    Self::$name(item)
                }
            }
        )*
    }
}

gen_scope_item!(Func, Ty, Spec, Var);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Pub,
    None,
    Priv,
}

impl Vis {
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Vis::None, vis) => vis,
            (vis, ..) => vis,
        }
    }
}

impl fmt::Display for Vis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Vis::Pub => write!(f, "pub"),
            Vis::Priv => write!(f, "priv"),
            Vis::None => write!(f, ""),
        }
    }
}

impl Default for Vis {
    fn default() -> Self {
        Vis::None
    }
}
