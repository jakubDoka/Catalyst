// use std::{sync::Arc, marker::PhantomData};

// use crate::Allocator;

// type Node = Arc<Allocator>;

// pub trait NodeItem {
//     type Instance<'a>;
// }

// pub struct UniqueNodeItem<I: NodeItem> {
//     item: *mut I,
//     node: Node,
//     _ph: PhantomData<I>
// }

// pub struct UniqueNode {
//     inner: Node,
// }

// impl UniqueNode {
//     pub fn new() -> Self {
//         Self {
//             inner: Node::new(Allocator::new()),
//         }
//     }

//     pub fn alloc<T>(&self, value: T) -> &T {
//         self.inner.alloc(value)
//     }

//     pub fn into_shared(self) -> SharedNode {
//         SharedNode { inner: self.inner }
//     }
// }

// pub struct SharedNode {
//     inner: Node,
// }
