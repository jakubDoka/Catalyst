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
    data: Map<VRef<str>, Maybe<Item>>,
    frames: Frames<(VRef<str>, Maybe<Item>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, ident: VRef<str>) -> Result<Item, ScopeError> {
        self.data
            .get(&ident)
            .map(|option| option.as_ref_option().ok_or(ScopeError::Collision))
            .ok_or(ScopeError::NotFound)
            .flatten()
            .cloned()
    }

    pub fn get_typed<T: 'static>(&self, ident: VRef<str>) -> Result<(VRef<T>, Item), ScopeError> {
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
        self.frames.push((
            item.id,
            self.data
                .insert(item.id, item.into())
                .unwrap_or(Maybe::none()),
        ));
    }

    pub fn start_frame(&mut self) {
        self.frames.mark();
    }

    pub fn end_frame(&mut self) {
        for (id, item) in self.frames.pop() {
            if let Some(item) = item.expand() {
                self.data.insert(id, item.into());
            } else {
                self.data.remove(&id);
            }
        }
    }

    pub fn insert_builtin(&mut self, id: VRef<str>, ptr: VRef<impl Any>) {
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
        current_module: VRef<str>,
        item: ScopeItem,
    ) -> Result<(), Option<(Span, Span)>> {
        if let Some(existing_option) = self.data.get_mut(&item.id) {
            let Some(existing) = existing_option.as_mut_option() else {
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
        self.frames.clear();
    }
}

pub enum ScopeError {
    NotFound,
    Collision,
    TypeMismatch(TypeId),
}

#[derive(Clone, Copy, Default)]
pub struct Item {
    pub id: VRef<str>,
    pub ptr: ScopePtr,
    pub span: Maybe<Span>,
    pub whole_span: Maybe<Span>,
    pub module: Maybe<VRef<str>>,
    pub vis: Vis,
}

impl Item {
    /// returns (self.whole_span, self.span)
    fn spans(&self) -> Option<(Span, Span)> {
        self.span.expand().and_then(|span| {
            self.whole_span
                .expand()
                .map(|whole_span| (whole_span, span))
        })
    }
}

impl Invalid for Item {
    unsafe fn invalid() -> Self {
        Item {
            id: VRef::invalid(),
            ptr: ScopePtr::invalid(),
            span: Maybe::none(),
            whole_span: Maybe::none(),
            module: Maybe::none(),
            vis: Vis::None,
        }
    }

    fn is_invalid(&self) -> bool {
        self.id.is_invalid()
    }
}

#[derive(Clone, Copy)]
pub struct ScopeItem {
    pub id: VRef<str>,
    pub ptr: ScopePtr,
    pub span: Span,
    pub whole_span: Span,
    pub module: VRef<str>,
    pub vis: Vis,
}

impl ScopeItem {
    pub fn new(
        id: VRef<str>,
        ptr: impl Into<ScopePtr>,
        span: Span,
        whole_span: Span,
        module: VRef<str>,
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

impl Into<Item> for ScopeItem {
    fn into(self) -> Item {
        Item {
            id: self.id,
            ptr: self.ptr,
            span: Maybe::some(self.span),
            whole_span: Maybe::some(self.whole_span),
            module: Maybe::some(self.module),
            vis: self.vis,
        }
    }
}

impl Into<Maybe<Item>> for ScopeItem {
    fn into(self) -> Maybe<Item> {
        Maybe::some(self.into())
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

impl Invalid for ScopePtr {
    unsafe fn invalid() -> Self {
        ScopePtr {
            id: TypeId::of::<InvalidItem>(),
            ptr: u32::MAX as usize,
        }
    }

    fn is_invalid(&self) -> bool {
        self.id == TypeId::of::<InvalidItem>()
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

struct InvalidItem;

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
    pub fn merge(self, other: Self) -> Self {
        match other {
            Vis::None => self,
            _ => other,
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
