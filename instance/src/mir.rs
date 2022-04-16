use std::{marker::PhantomData, ops::IndexMut};

use cranelift_codegen::{packed_option::PackedOption, ir::Type};
use cranelift_entity::{EntityRef, PrimaryMap, EntityList};
use typec::Ty;

use crate::Size;

impl<E: EntityRef, N: LinkedNode<E>> LinkedList<E, N> for PrimaryMap<E, N> {}

#[macro_export]
macro_rules! impl_linked_node {
    ($entity:ty, $target:ty) => {
        impl $crate::mir::LinkedNode<$entity> for $target {
            fn prev(&self) -> Option<$entity> {
                self.prev.expand()
            }

            fn next(&self) -> Option<$entity> {
                self.next.expand()
            }
        }
    };

    (mut $entity:ty, $target:ty) => {
        impl $crate::mir::MutLinkNode<$entity> for $target {
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

#[derive(Default, Clone, Copy, Debug)]
pub struct BlockEnt {
    pub prev: PackedOption<Block>,
    pub next: PackedOption<Block>,
    pub start: PackedOption<Inst>,
    pub end: PackedOption<Inst>,
    pub params: EntityList<Value>,
}

crate::impl_linked_node!(inout Block, BlockEnt);
lexer::gen_entity!(Block);

#[derive(Debug)]
pub struct InstEnt {
    pub prev: PackedOption<Inst>,
    pub next: PackedOption<Inst>,
    pub kind: InstKind,
    pub value: PackedOption<Value>,
}

impl InstEnt {
    pub fn valueless(kind: InstKind) -> Self {
        Self::new(kind, None)
    }

    pub fn with_value(kind: InstKind, value: Value) -> Self {
        Self::new(kind, value.into())
    }

    pub fn new(kind: InstKind, value: Option<Value>) -> Self {
        Self {
            prev: None.into(),
            next: None.into(),
            kind,
            value: value.into(),
        }
    }
}

#[derive(Debug)]
pub enum InstKind {
    Offset(Value),
    StackAddr(Stack),
    Variable,
    Assign(Value),
    JumpIfFalse(Block),
    Jump(Block),
    Call(typec::Func, EntityList<Value>),
    IntLit(u64),
    BoolLit(bool),
    Return,
}

impl InstKind {
    pub fn is_terminating(&self) -> bool {
        matches!(self, Self::Return | Self::Jump(_))
    }
}

crate::impl_linked_node!(inout Inst, InstEnt);
lexer::gen_entity!(Inst);

#[derive(Debug, Clone, Copy)]
pub struct ValueEnt {
    pub ty: Ty,
    pub offset: Size,
    pub flags: Flags,
}

impl ValueEnt {
    pub fn new(ty: Ty, offset: Size, flags: Flags) -> ValueEnt {
        ValueEnt {
            ty,
            offset,
            flags,
        }
    }

    pub fn repr(ty: Ty) -> Self {
        Self::new(ty, Size::ZERO, Flags::default())
    }

    pub fn offset(ty: Ty, offset: Size) -> Self {
        Self::new(ty, offset, Flags::default())
    }

    pub fn flags(ty: Ty,flags: Flags) -> Self {
        Self::new(ty, Size::ZERO, flags)
    }
}

lexer::gen_entity!(Value);

bitflags::bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        /// The value is a pointer.
        const POINTER = 1 << 0;
        /// The value is mutable.
        const MUTABLE = 1 << 1;
        /// The value is unsigned integer.
        const UNSIGNED = 1 << 2;
    }
}

typec::impl_bool_bit_and!(Flags);

#[derive(Debug, Clone, Copy)]
pub struct StackEnt {
    pub size: u32,
}

impl StackEnt {
    pub fn new(size: Size, ptr_ty: Type) -> Self {
        Self { size: size.arch(ptr_ty.bytes() == 4) as u32 }
    }
}

lexer::gen_entity!(Stack);