use std::{any::TypeId, collections::HashMap};

use crate::*;
use cranelift_entity::{packed_option::ReservedValue, EntityRef};
use lexer::*;

pub struct ItemLexicon {
    map: HashMap<TypeId, &'static str>,
}

impl ItemLexicon {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn register<T: 'static>(&mut self, name: &'static str) {
        self.map.insert(TypeId::of::<T>(), name);
    }

    pub fn name_of(&self, item: TypeId) -> &str {
        self.map.get(&item).unwrap()
    }
}

pub struct Scope {
    map: Map<Item>,
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

    pub fn collision_rate(&self) -> f64 {
        self.map.collision_rate()
    }

    pub fn weak_get<T: 'static + EntityRef>(&self, id: impl Into<ID>) -> Option<T> {
        self.map
            .get(id)
            .map(|item| item.pointer.may_read())
            .flatten()
    }

    pub fn get<T: 'static + EntityRef>(
        &self,
        diagnostics: &mut errors::Diagnostics,
        id: impl Into<ID>,
        span: Span,
    ) -> errors::Result<T> {
        self.get_by_id(diagnostics, id.into(), span)
    }

    pub fn may_get<T: 'static + EntityRef>(
        &self,
        diagnostics: &mut errors::Diagnostics,
        id: impl Into<ID>,
        span: Span,
    ) -> errors::Result<Option<T>> {
        self.may_get_by_id(diagnostics, id.into(), span)
    }

    pub fn get_by_id<T: 'static + EntityRef>(
        &self,
        diagnostics: &mut errors::Diagnostics,
        id: ID,
        span: Span,
    ) -> errors::Result<T> {
        let Some(data) = self.may_get_by_id(diagnostics, id, span)? else {
            diagnostics.push(Error::InvalidScopeItem {
                loc: span,
                expected: TypeId::of::<T>(),
                found: self.map.get(id).unwrap().pointer.id,
            });
            return Err(());
        };

        Ok(data)
    }

    pub fn may_get_by_id<T: 'static + EntityRef>(
        &self,
        diagnostics: &mut errors::Diagnostics,
        id: ID,
        span: Span,
    ) -> errors::Result<Option<T>> {
        let Some(item) = self.map.get(id) else {
            diagnostics.push(Error::ScopeItemNotFound {
                loc: span,
            });
            return Err(());
        };

        if item.pointer.is_of::<Collision>() {
            let suggestions = self
                .dependencies
                .iter()
                .filter_map(|&(source, span)| self.map.get((id, source)).map(|_| span))
                .collect::<Vec<_>>();

            diagnostics.push(Error::AmbiguousScopeItem {
                loc: span,
                suggestions,
            });

            return Err(());
        }

        Ok(item.pointer.may_read::<T>())
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

    pub fn pop_item(&mut self) {
        assert!(
            *self.frames.last().unwrap() <= self.stack.len(),
            "{:?} < {}",
            self.frames.last(),
            self.stack.len()
        );
        self.stack.pop().unwrap();
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

    pub fn insert(
        &mut self,
        diagnostics: &mut errors::Diagnostics,
        current_source: Source,
        id: impl Into<ID>,
        item: Item,
    ) -> errors::Result {
        self.insert_by_id(diagnostics, current_source, id.into(), item)
    }

    fn insert_by_id(
        &mut self,
        diagnostics: &mut errors::Diagnostics,
        current_source: Source,
        id: ID,
        item: Item,
    ) -> errors::Result {
        let item_source = item.span.source();

        match self.map.insert(id, Item::collision()) {
            Some(colliding) => {
                let colliding_source = colliding.span.source();
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
                        diagnostics.push(Error::ScopeCollision {
                            new: item.span,
                            existing: colliding.span,
                        });
                        return Err(());
                    }
                } else {
                    assert!(self.map.insert((id, item_source), item).is_none());
                    assert!(
                        self.map.insert((id, colliding_source), colliding).is_none(),
                        "{:?}",
                        colliding
                    );
                }
            }
            None => assert!(self.map.insert(id, item) == Some(Item::collision())),
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
    pub shadow: Option<Item>,
}

impl ScopeSlot {
    pub fn new(id: ID, shadow: Option<Item>) -> Self {
        Self { id, shadow }
    }
}

#[derive(Default, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Item {
    pub span: Span,
    pub pointer: Pointer,
}

impl Item {
    pub fn new(data: impl 'static + EntityRef, span: Span) -> Self {
        Self {
            span,
            pointer: Pointer::write(data),
        }
    }

    fn collision() -> Item {
        Item {
            span: Span::new(Source(0), 0, 0),
            pointer: Pointer::write(Collision),
        }
    }
}

impl ReservedValue for Item {
    fn reserved_value() -> Self {
        Self::default()
    }

    fn is_reserved_value(&self) -> bool {
        self.span.source().is_reserved_value()
    }
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
