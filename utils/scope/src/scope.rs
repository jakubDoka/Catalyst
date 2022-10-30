use core::fmt;
use std::{
    any::{Any, TypeId},
    default::default,
    fmt::{Display, Formatter},
};

use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct Scope {
    data: Map<FragSlice<u8>, Option<Item>>,
    pushed: Vec<(FragSlice<u8>, Option<Item>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, ident: FragSlice<u8>) -> Result<Item, ScopeError> {
        self.data
            .get(&ident)
            .map(|option| option.as_ref().ok_or(ScopeError::Collision))
            .ok_or(ScopeError::NotFound)
            .flatten()
            .cloned()
    }

    pub fn get_typed<T: 'static>(
        &self,
        ident: FragSlice<u8>,
    ) -> Result<(VRef<T>, Item), ScopeError> {
        let item = self.get(ident)?;
        Ok((
            item.ptr
                .try_read::<T>()
                .ok_or(ScopeError::TypeMismatch(item.ptr.id))?,
            item,
        ))
    }

    pub fn push(&mut self, item: impl Into<Item>) {
        let item: Item = item.into();
        self.pushed
            .push((item.id, self.data.insert(item.id, item.into()).flatten()));
    }

    pub fn start_frame(&mut self) -> ScopeFrame {
        ScopeFrame(self.pushed.len())
    }

    pub fn end_frame(&mut self, frame: ScopeFrame) {
        for (id, item) in self.pushed.drain(frame.0..) {
            if let Some(item) = item {
                self.data.insert(id, item.into());
            } else {
                self.data.remove(&id);
            }
        }
    }

    pub fn insert_builtin(&mut self, id: FragSlice<u8>, ptr: VRef<impl Any>) {
        self.data.insert(
            id,
            Item {
                id,
                ptr: ScopePtr::new(ptr),
                ..default()
            }
            .into(),
        );
    }

    pub fn insert_current(&mut self, item: ScopeItem) -> Result<(), Option<(Span, Span)>> {
        self.insert(item.module, item)
    }

    pub fn insert(
        &mut self,
        current_module: FragSlice<u8>,
        item: ScopeItem,
    ) -> Result<(), Option<(Span, Span)>> {
        if let Some(existing_option) = self.data.get_mut(&item.id) {
            let Some(existing) = existing_option.as_mut() else {
                if current_module == item.module {
                    *existing_option = item.into();
                }
                return Ok(());
            };

            if existing.module == item.module.into() || existing.module.is_none() {
                return Err(existing.spans());
            }

            if item.module == current_module {
                *existing = item.into();
            } else if existing.module != current_module.into() {
                existing_option.take();
            }
        } else {
            self.data.insert(item.id, item.into());
        }

        Ok(())
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
    TypeMismatch(TypeId),
}

#[derive(Clone, Copy, Default)]
pub struct Item {
    pub id: FragSlice<u8>,
    pub ptr: ScopePtr,
    pub span: Option<Span>,
    pub whole_span: Option<Span>,
    pub module: Option<FragSlice<u8>>,
    pub vis: Vis,
}

impl Item {
    /// returns (self.whole_span, self.span)
    fn spans(&self) -> Option<(Span, Span)> {
        self.span
            .and_then(|span| self.whole_span.map(|whole_span| (whole_span, span)))
    }
}

#[derive(Clone, Copy)]
pub struct ScopeItem {
    pub id: FragSlice<u8>,
    pub ptr: ScopePtr,
    pub span: Span,
    pub whole_span: Span,
    pub module: FragSlice<u8>,
    pub vis: Vis,
}

impl ScopeItem {
    pub fn new(
        id: FragSlice<u8>,
        ptr: impl Into<ScopePtr>,
        span: Span,
        whole_span: Span,
        module: FragSlice<u8>,
        vis: Vis,
    ) -> Self {
        Self {
            id,
            ptr: ptr.into(),
            span,
            whole_span,
            module,
            vis,
        }
    }
}

impl From<ScopeItem> for Item {
    fn from(item: ScopeItem) -> Self {
        Item {
            id: item.id,
            ptr: item.ptr,
            span: Some(item.span),
            whole_span: Some(item.whole_span),
            module: Some(item.module),
            vis: item.vis,
        }
    }
}

impl From<ScopeItem> for Option<Item> {
    fn from(item: ScopeItem) -> Self {
        Some(item.into())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ScopePtr {
    pub id: TypeId,
    pub ptr: usize,
}

impl ScopePtr {
    pub fn new<T: 'static>(id: VRef<T>) -> Self {
        Self {
            id: TypeId::of::<T>(),
            ptr: id.index(),
        }
    }

    pub fn is_of<T: 'static>(&self) -> bool {
        self.id == TypeId::of::<T>()
    }

    pub fn try_read<T: 'static>(&self) -> Option<VRef<T>> {
        self.is_of::<T>().then_some(unsafe { VRef::new(self.ptr) })
    }

    pub fn read<T: 'static>(&self) -> VRef<T> {
        self.try_read::<T>().unwrap()
    }
}

impl Default for ScopePtr {
    fn default() -> Self {
        Self {
            id: TypeId::of::<()>(),
            ptr: 0,
        }
    }
}

#[macro_export]
macro_rules! match_scope_ptr {
    ($ptr:expr =>
        $(
            $binding:ident: $ty:ty => $expr:expr,
        )*
        _ => $default:expr,
    ) => {
        {
            let ptr = $ptr;
            $(
                if let Some($binding) = ptr.try_read::<$ty>() {
                    $expr
                } else
            )*
            {
                $default
            }
        }
    };
}

#[cfg(test)]
#[test]
fn test_match_scope_ptr() {
    let res = match_scope_ptr! {ScopePtr::default() =>
        _a: () => 1,
        _ => 0,
    };

    assert!(res == 1);
}

impl<T: 'static> From<VRef<T>> for ScopePtr {
    fn from(value: VRef<T>) -> Self {
        Self::new(value)
    }
}

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

impl Display for Vis {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
