use lexing_t::*;
use packaging_t::Module;
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
    ) {
        debug_assert!(current_module != foreign_module);

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
            Self::ImportedItem { item, .. } | Self::CurrentItem { item } => Some(item.item),
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

#[derive(Clone, Copy)]
pub struct ModuleItem {
    pub id: VRef<str>,
    pub item: ScopeItem,
    pub span: Span,
    pub vis: Vis,
}

impl ModuleItem {
    pub fn new(id: VRef<str>, ptr: impl Into<ScopeItem>, span: Span, vis: Vis) -> Self {
        Self {
            id,
            item: ptr.into(),
            span,
            vis,
        }
    }
}

#[derive(Clone, Copy)]
pub enum ScopeItem {
    Func(VRef<Func>),
    Ty(VRef<Ty>),
    Spec(VRef<Spec>),
    Var(VRef<Var>),
    Module(VRef<Module>),
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

        impl ScopeItem {
           pub fn what(&self) -> &'static str {
                match *self {
                    $(Self::$name(..) => stringify!($name),)*
                }
            }
        }
    }
}

gen_scope_item!(Func, Ty, Spec, Var, Module);
