use std::any::TypeId;

use lexing_t::*;
use storage::*;

pub struct Scope {
    data: Map<Maybe<Item>>,
    frames: Frames<(Ident, Maybe<Item>)>,
    pub self_alias: Maybe<Ident>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn project(&self, id: Ident, str: &str) -> Ident {
        self.self_alias
            .expand()
            .and_then(|s| (str == "Self").then_some(s))
            .unwrap_or(id)
    }

    pub fn get(&self, ident: Ident) -> Result<Item, ScopeError> {
        self.data
            .get(ident)
            .map(|option| option.as_ref_option().ok_or(ScopeError::Collision))
            .ok_or(ScopeError::NotFound)
            .flatten()
            .cloned()
    }

    pub fn get_concrete<T: VPtr + 'static>(&self, ident: Ident) -> Result<(T, Item), ScopeError> {
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
                self.data.remove(id);
            }
        }
    }

    pub fn insert_builtin(&mut self, id: Ident, ptr: impl VPtr + 'static) {
        self.data.insert_unique(
            id,
            Item {
                id,
                ptr: ScopePtr::new(ptr),
                span: Maybe::none(),
                module: Maybe::none(),
                vis: Vis::None,
            }
            .into(),
        );
    }

    pub fn insert_current(
        &mut self,
        item: ScopeItem,
        interner: &mut Interner,
    ) -> Result<(), Maybe<Span>> {
        self.insert(item.id, item, interner)
    }

    pub fn insert(
        &mut self,
        module: Ident,
        item: ScopeItem,
        interner: &mut Interner,
    ) -> Result<(), Maybe<Span>> {
        if item.module != module {
            let id = interner.intern(scoped_ident!(item.module, item.id));
            self.data.insert_unique(id, item.into());
        }

        if let Some(existing_option) = self.data.get_mut(item.id)
            && let Some(existing) = existing_option.as_mut_option()
        {
            if existing.module == item.module.into() || existing.module.is_none() {
                return Err(existing.span);
            }

            if item.module == module {
                *existing = item.into();
            } else if existing.module != module.into() {
                existing_option.take();
            }
        } else {
            self.data.insert_unique(item.id, item.into());
        }

        Ok(())
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.frames.clear();
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            data: Map::new(),
            frames: Frames::new(),
            self_alias: Maybe::none(),
        }
    }
}

pub enum ScopeError {
    NotFound,
    Collision,
    TypeMismatch(TypeId),
}

#[derive(Clone, Copy)]
pub struct Item {
    pub id: Ident,
    pub ptr: ScopePtr,
    pub span: Maybe<Span>,
    pub module: Maybe<Ident>,
    pub vis: Vis,
}

impl Invalid for Item {
    unsafe fn invalid() -> Self {
        Item {
            id: Ident::invalid(),
            ptr: ScopePtr::invalid(),
            span: Maybe::none(),
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
    pub id: Ident,
    pub ptr: ScopePtr,
    pub span: Span,
    pub module: Ident,
    pub vis: Vis,
}

impl ScopeItem {
    pub fn new(id: Ident, ptr: impl Into<ScopePtr>, span: Span, module: Ident, vis: Vis) -> Self {
        Self {
            id,
            ptr: ptr.into(),
            span,
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

#[derive(Clone, Copy)]
pub struct ScopePtr {
    pub id: TypeId,
    pub ptr: usize,
}

impl ScopePtr {
    pub fn new<T: VPtr + 'static>(id: T) -> Self {
        Self {
            id: TypeId::of::<T>(),
            ptr: id.index(),
        }
    }

    pub fn is_of<T: VPtr + 'static>(&self) -> bool {
        self.id == TypeId::of::<T>()
    }

    pub fn try_read<T: VPtr + 'static>(&self) -> Option<T> {
        self.is_of::<T>().then_some(T::new(self.ptr))
    }

    pub fn read<T: VPtr + 'static>(&self) -> T {
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

struct InvalidItem;

impl<T: VPtr + 'static> From<T> for ScopePtr {
    fn from(value: T) -> Self {
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

impl Default for Vis {
    fn default() -> Self {
        Vis::None
    }
}
