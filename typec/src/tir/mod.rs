use std::{marker::PhantomData, ops::IndexMut};

use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityRef, PrimaryMap,
};

pub mod block;
pub mod inst;
pub mod value;

pub use block::*;
pub use inst::*;
pub use value::*;

impl<E: EntityRef, N: LinkedNode<E>> LinkedList<E, N> for PrimaryMap<E, N> {}

#[macro_export]
macro_rules! impl_linked_node {
    ($entity:ty, $target:ty) => {
        impl $crate::tir::LinkedNode<$entity> for $target {
            fn prev(&self) -> Option<$entity> {
                self.prev.expand()
            }

            fn next(&self) -> Option<$entity> {
                self.next.expand()
            }
        }
    };

    (mut $entity:ty, $target:ty) => {
        impl $crate::tir::MutLinkNode<$entity> for $target {
            fn set_prev(&mut self, prev: Option<$entity>) {
                self.prev = prev.into();
            }

            fn set_next(&mut self, next: Option<$entity>) {
                self.next = next.into();
            }
        }
    };

    (inout $entity:ty, $target:ty) => {
        $crate::impl_linked_node!($entity, $target);
        $crate::impl_linked_node!(mut $entity, $target);
    };
}

pub trait LinkedNode<E: EntityRef>: 'static {
    fn prev(&self) -> Option<E>;
    fn next(&self) -> Option<E>;
}

pub trait MutLinkNode<E: EntityRef>: LinkedNode<E> {
    fn set_prev(&mut self, prev: Option<E>);
    fn set_next(&mut self, next: Option<E>);
}

pub trait LinkedList<E: EntityRef, N: LinkedNode<E>>: Sized + IndexMut<E, Output = N> {
    fn linked_iter(&self, root: Option<E>) -> LinkedIter<Self, E, N> {
        LinkedIter {
            storage: self,
            current: root,
            _ph: PhantomData,
        }
    }

    fn linked_iter_mut(&mut self, root: Option<E>) -> LinkedIterMut<Self, E, N> {
        LinkedIterMut {
            storage: self,
            current: root,
            _ph: PhantomData,
        }
    }

    fn insert(
        &mut self,
        target: E,
        under: Option<E>,
        start: &mut PackedOption<E>,
        end: &mut PackedOption<E>,
    ) where
        N: MutLinkNode<E>,
        E: ReservedValue,
    {
        if let Some(under) = under {
            let next = self[under].next();
            self[under].set_next(Some(target).into());
            self[target].set_prev(Some(under).into());
            self[target].set_next(next);
            if let Some(next) = next {
                self[next].set_prev(Some(target).into());
            }
        } else {
            *start = Some(target).into();
        }
        *end = Some(target).into();
    }
}

macro_rules! gen_iter {
    ($name:ident, $($mutability:tt)?) => {
        pub struct $name<'a, S, E, N> {
            storage: &'a $($mutability)? S,
            current: Option<E>,
            _ph: PhantomData<N>,
        }

        impl<'a, S: LinkedList<E, N>, E: EntityRef, N: LinkedNode<E>> Iterator for $name<'a, S, E, N> {
            type Item = (E, &'a $($mutability)? N);

            fn next(&mut self) -> Option<Self::Item> {
                self.current.map(|current_ent| {
                    let current = unsafe { std::mem::transmute::<_, &'a $($mutability)? N>(& $($mutability)? self.storage[current_ent]) };
                    self.current = current.next();
                    (current_ent, current)
                })
            }
        }
    };
}

gen_iter!(LinkedIter,);
gen_iter!(LinkedIterMut, mut);
