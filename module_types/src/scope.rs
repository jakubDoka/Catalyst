use std::{any::TypeId, collections::hash_map::Entry};

use crate::{error::*, ModuleItem};
use lexer::*;
use storage::*;

#[derive(Debug, Clone)]
pub enum ScopeFindError {
    NotFound,
    InvalidType(TypeId),
    Collision(Vec<Span>),
    Other,
}

pub struct Scope {
    map: Map<ScopeItem>,
    frames: Vec<usize>,
    stack: Vec<ScopeSlot>,
    pub dependencies: Vec<(Source, Span)>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: Map::new(),
            frames: Vec::new(),
            stack: Vec::new(),
            dependencies: Vec::new(),
        }
    }

    pub fn get_concrete<T: EntityRef + 'static>(
        &self,
        id: impl Into<ID>,
    ) -> Result<T, ScopeFindError> {
        self.get_concrete_by_id(id.into())
    }

    pub fn get_concrete_by_id<T: EntityRef + 'static>(
        &self,
        id: impl Into<ID>,
    ) -> Result<T, ScopeFindError> {
        let id = id.into();
        self.get_by_id(id).and_then(|item| {
            item.pointer
                .may_read::<T>()
                .ok_or(ScopeFindError::InvalidType(item.pointer.id))
        })
    }

    pub fn get(&self, id: impl Into<ID>) -> Result<ScopeItem, ScopeFindError> {
        self.get_by_id(id.into())
    }

    pub fn get_by_id(&self, id: ID) -> Result<ScopeItem, ScopeFindError> {
        self.map
            .get(id)
            .cloned()
            .ok_or(ScopeFindError::NotFound)
            .and_then(|item| {
                item.pointer
                    .may_read::<ScopeCollision>()
                    .is_none()
                    .then_some(item)
                    .ok_or_else(|| ScopeFindError::Collision(self.suggestions(id.into())))
            })
    }

    pub fn suggestions(&self, id: ID) -> Vec<Span> {
        self.dependencies
            .iter()
            .filter_map(|&(source, span)| self.map.get(ID::scoped(id, source)).map(|_| span))
            .collect::<Vec<_>>()
    }

    pub fn mark_frame(&mut self) {
        self.frames.push(self.stack.len());
    }

    pub fn push_item(&mut self, id: impl Into<ID>, item: ScopeItem) {
        let id = id.into();
        self.stack
            .push(ScopeSlot::new(id, self.map.insert(id, item)));
    }

    pub fn pop_item(&mut self) {
        assert!(
            *self.frames.last().unwrap() <= self.stack.len(),
            "{:?} <= {}",
            self.frames.last(),
            self.stack.len()
        );
        let item = self.stack.pop().unwrap();
        if let Some(shadow) = item.shadow {
            self.map.insert(item.id, shadow);
        } else {
            assert!(self.map.remove(item.id).is_some());
        }
    }

    pub fn pop_frame(&mut self) {
        let len = self.frames.pop().unwrap();
        self.stack.drain(len..).for_each(|slot| {
            if let Some(shadow) = slot.shadow {
                self.map.insert(slot.id, shadow);
            } else {
                assert!(self.map.remove(slot.id).is_some());
            }
        });
    }

    pub fn insert_current(&mut self, diagnostics: &mut errors::Diagnostics, item: ModuleItem) {
        self.insert(diagnostics, item.span.source(), item)
    }

    pub fn insert(
        &mut self,
        diagnostics: &mut errors::Diagnostics,
        current_source: Source,
        item: ModuleItem,
    ) {
        drop(self.insert_by_id(diagnostics, current_source, item.id, item.to_scope_item()))
    }

    pub fn insert_builtin(&mut self, item: ModuleItem) {
        let id = item.id;
        let item = item.to_scope_item();
        assert!(self.map.insert(id, item).is_none())
    }

    fn insert_by_id(
        &mut self,
        diagnostics: &mut errors::Diagnostics,
        current_source: Source,
        id: ID,
        item: ScopeItem,
    ) -> errors::Result {
        let item_source = item.span.source();

        match self.map.entry(id) {
            Entry::Occupied(mut entry) => {
                let colliding = entry.get().clone();
                let colliding_source = colliding.span.source();
                if colliding.pointer.is_of::<ScopeCollision>() {
                    if item_source == current_source {
                        entry.insert(item);
                    } else {
                        assert!(self.map.insert(ID::scoped(id, item_source), item).is_none());
                    }
                } else if colliding_source == current_source {
                    if item_source != current_source {
                        entry.insert(ScopeItem::collision());
                        assert!(self.map.insert(id, colliding) == Some(ScopeItem::collision()));
                        assert!(self.map.insert(ID::scoped(id, item_source), item).is_none());
                    } else {
                        diagnostics.push(ModuleError::ScopeCollision {
                            new: item.span,
                            existing: colliding.span,
                        });
                        return Err(());
                    }
                } else {
                    assert!(self.map.insert(ID::scoped(id, item_source), item).is_none());
                    assert!(
                        self.map
                            .insert(ID::scoped(id, colliding_source), colliding)
                            .is_none(),
                        "{:?}",
                        colliding
                    );
                }
            }
            Entry::Vacant(entry) => drop(entry.insert(item)),
        }

        Ok(())
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.frames.clear();
        self.stack.clear();
        self.dependencies.clear();
    }
}

pub struct ScopeSlot {
    pub id: ID,
    pub shadow: Option<ScopeItem>,
}

impl ScopeSlot {
    pub fn new(id: ID, shadow: Option<ScopeItem>) -> Self {
        Self { id, shadow }
    }
}

#[derive(Default, PartialEq, Eq, Debug, Clone, Copy)]
pub struct ScopeItem {
    pub span: Span,
    pub pointer: ScopePointer,
}

impl ScopeItem {
    pub fn new(data: impl 'static + EntityRef, span: Span) -> Self {
        Self {
            span,
            pointer: ScopePointer::write(data),
        }
    }

    fn collision() -> ScopeItem {
        ScopeItem {
            span: Span::new(Source(0), 0, 0),
            pointer: ScopePointer::write(ScopeCollision),
        }
    }
}

impl ReservedValue for ScopeItem {
    fn reserved_value() -> Self {
        Self::default()
    }

    fn is_reserved_value(&self) -> bool {
        self.span.source().is_reserved_value()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ScopePointer {
    pub id: TypeId,
    data: usize,
}

impl ScopePointer {
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

impl Default for ScopePointer {
    fn default() -> Self {
        Self {
            id: TypeId::of::<()>(),
            data: 0,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct ScopeCollision;

impl EntityRef for ScopeCollision {
    fn index(self) -> usize {
        0
    }

    fn new(_: usize) -> Self {
        Self
    }
}
