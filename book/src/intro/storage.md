# Storage Strategy

One aspect I struggled with is the way one would manage dataflow in a Compiler.
Code tightly depends on how easy it is to access data needed for computations.
It is extra tough for compilers since cycles in data are common and rust does
not allow you to do that easily.

In this chapter we will go over ways to represent compiler datastructures.

## Owned Tree of Types

Most common and least flexible way to do things is just represent things as a
tree. This works kind of ok for **Abstract Syntac Tree** (AST), but fails
terribly when you cannot just have a tree. After tree comes aciclyc graph.
This is usualy prematurely solved by `Rc` and `Arc` which also makes cloning
things cheaper, though, shared smart pointers are immutable, which brings new
class of problems upon us. Should we use `RefCell` and `Mutex` then?

Lets go straight to the point. Creating self referential systems
**with pointers** in safe `Rust`is a nightmare. Can we do better then this?

## Index Referencing

Way to solve Cycles and ownership problems is rather simple. We can allocate
our data into a vector and refer to it by index. To make this more managable,
we dont just use usize for everithing but make an encapsulated vector that
returns type implementing trait `EntityId` form `push` pointing to the pushed
entity. We can then index the vector with this id to get access to data. The
storage ends up something like `EntityVec<E: EntityId, D>`. Since `E` is `Copy`
we can put it into copy structures. We can create cycles and have multiple
`E`s to single entity.

## Better Index Referencing

As mentioned, `EntityVec` is generic over id and data. A little upgrade to this
model can be done by `VRef<T>` where `T` is the data `VRef` points to. It can
be implemented as following:

```rust
struct VRef<T>(u32, PhantomData<T>);
```

`EntityVec` how needs just `T` and returns `VRef<T>`. Now that we perfected
the indexing, another problem is upon us. How do we share and update
`EntityVec` from multiple threads? `Mutex` can save us from compiler errors
but it yelds far from optimal performance for compiler sym table. We need to
get smarter and use some unsafe.

## Lock Free Strategy

All global language objects need to be available whenever user imports them,
but thay don't nesscesarly need to be writable. We also know that when we are
parsing and analizing a file, no other thread will care about it (no cycles
between packages or modules). Though we need to allow reallocation of the 
vectors.

From all of mentioned requirements, I derived a strategy that requires
minimal amount of synchronization. At the core of all this is ref-counted
allocation.