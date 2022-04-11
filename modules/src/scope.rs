use std::{any::TypeId, collections::HashMap};

use cranelift_entity::{packed_option::ReservedValue, EntityRef};
use lexer::{
    map::{Map, ID},
    {Source, Span},
};

use crate::error::{self, Error};

pub type ScopeItemLexicon = HashMap<TypeId, &'static str>;

pub struct Scope {
    map: Map<Item>,
    frames: Vec<usize>,
    stack: Vec<ScopeSlot>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: Map::new(),
            frames: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn get<T: 'static + EntityRef>(&self, id: impl Into<ID>, span: Span) -> Result<T, Error> {
        self.get_by_id(id.into(), span)
    }

    pub fn get_by_id<T: 'static + EntityRef>(&self, id: ID, span: Span) -> Result<T, Error> {
        self.map
            .get(id)
            .map(|item| {
                item.pointer.may_read::<T>().ok_or_else(|| {
                    if item.pointer.is_of::<Collision>() {
                        Error::new(error::Kind::ScopeItemCollision(id), span)
                    } else {
                        Error::new(
                            error::Kind::ScopeItemMismatch(TypeId::of::<T>(), item.pointer.id),
                            span,
                        )
                    }
                })
            })
            .ok_or_else(|| Error::new(error::Kind::ScopeItemNotFound, span))
            .flatten()
    }

    pub fn push_frame(&mut self, frame: impl Iterator<Item = (impl Into<ID>, Item)>) {
        self.mark_frame();
        for (id, item) in frame {
            self.push_item(id, item);
        }
    }

    pub fn mark_frame(&mut self) {
        self.frames.push(self.stack.len());
    }

    pub fn push_item(&mut self, id: impl Into<ID>, item: Item) {
        let id = id.into();
        self.stack
            .push(ScopeSlot::new(id, self.map.insert(id, item)));
    }

    pub fn pop_frame(&mut self) {
        let len = self.frames.pop().unwrap();
        self.stack.drain(len..).for_each(|slot| {
            if let Some(shadow) = slot.shadow {
                assert!(self.map.insert(slot.id, shadow).is_some());
            } else {
                assert!(self.map.remove(slot.id).is_some());
            }
        });
    }

    pub fn insert(
        &mut self,
        current_source: Source,
        id: impl Into<ID>,
        item: Item,
    ) -> Result<(), Error> {
        self.insert_by_id(current_source, id.into(), item)
    }

    fn insert_by_id(&mut self, current_source: Source, id: ID, item: Item) -> Result<(), Error> {
        let item_source = item.info.span.source();

        match self.map.insert(id, Item::collision()) {
            Some(colliding) => {
                let colliding_source = colliding.info.span.source();
                if colliding.pointer.is_of::<Collision>() {
                    if item_source == current_source {
                        assert!(self.map.insert(id, item) == Some(Item::collision()));
                    } else {
                        assert!(self.map.insert((id, item_source), item).is_none());
                    }
                } else if colliding_source == current_source {
                    if item_source != current_source {
                        assert!(self.map.insert(id, colliding) == Some(Item::collision()));
                        assert!(self.map.insert((id, item_source), item).is_none());
                    } else {
                        return Err(Error::new(
                            error::Kind::ScopeCollision(colliding.info.span),
                            item.info.span,
                        ));
                    }
                } else {
                    assert!(self.map.insert((id, item_source), item).is_none());
                    assert!(self.map.insert((id, colliding_source), colliding).is_none());
                }
            }
            None => assert!(self.map.insert(id, item).is_none()),
        }

        Ok(())
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.frames.clear();
        self.stack.clear();
    }
}

pub struct ScopeSlot {
    pub id: ID,
    pub shadow: Option<Item>,
}

impl ScopeSlot {
    pub fn new(id: ID, shadow: Option<Item>) -> Self {
        Self { id, shadow }
    }
}

#[derive(Default, PartialEq, Eq)]
pub struct Item {
    pub info: Info,
    pub pointer: Pointer,
}

impl Item {
    pub fn new(data: impl 'static + EntityRef, span: Span) -> Self {
        Self {
            info: Info { span },
            pointer: Pointer::write(data),
        }
    }

    fn collision() -> Item {
        Item {
            info: Default::default(),
            pointer: Pointer::write(Collision),
        }
    }
}

impl ReservedValue for Item {
    fn reserved_value() -> Self {
        Self::default()
    }

    fn is_reserved_value(&self) -> bool {
        self.info.span.source().is_reserved_value()
    }
}

#[derive(Default, PartialEq, Eq)]
pub struct Info {
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Pointer {
    id: TypeId,
    data: usize,
}

impl Pointer {
    pub fn write<T: 'static + EntityRef>(data: T) -> Self {
        Self {
            id: TypeId::of::<T>(),
            data: data.index(),
        }
    }

    pub fn is_of<T: 'static + EntityRef>(&self) -> bool {
        self.id == TypeId::of::<T>()
    }

    pub fn read<T: 'static + EntityRef>(&self) -> T {
        self.may_read().unwrap()
    }

    pub fn may_read<T: 'static + EntityRef>(&self) -> Option<T> {
        if self.is_of::<T>() {
            Some(T::new(self.data))
        } else {
            None
        }
    }
}

impl Default for Pointer {
    fn default() -> Self {
        Self {
            id: TypeId::of::<()>(),
            data: 0,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Collision;

impl EntityRef for Collision {
    fn index(self) -> usize {
        0
    }

    fn new(_: usize) -> Self {
        Self
    }
}
