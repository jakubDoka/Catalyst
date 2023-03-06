# Storage Strategy

One aspect I struggled with is the way one would manage data flow in a Compiler.
Code tightly depends on how easy it is to access data needed for computations.
It is extra tough for compilers since cycles in data are common and rust does
not allow you to do that easily.

In this chapter, we will go over ways to represent compiler data structures.

## Owned Tree of Types

The most common and least flexible way to do things is just represented things as a
tree. This works kind of ok for **Abstract Syntax Tree** (AST), but fails
terribly when you cannot just have a tree. After tree comes acyclic graph.
This is usually prematurely solved by and which also makes cloning
things cheaper, though, shared smart pointers are immutable, which brings a new
class of problems upon us. Should we use and then?

Let's go straight to the point. Creating self-referential systems
**with pointers** in safe `Rust`is a nightmare. Can we do better than this?

## Index Referencing

The way to solve Cycles and ownership problems is rather simple. We can allocate
our data into a vector and refer to it by index. To make this more manageable,
we don't just use usize for everything, but make an encapsulated vector that
returns a type implementing trait form pointing to the pushed
entity. We can then index the vector with this ID to get access to data. The
storage ends up something like. Since is `Copy`
we can put it into copy structures. We can create cycles and have multiple
`E`s to a single entity.

## Better Index Referencing

As mentioned, is generic over ID and data. A little upgrade to this
model can be done by where is the data points to. It can
be implemented as following:

```rust
struct VRef<T>(u32, PhantomData<T>);
```

`EntityVec` now needs just and returns. Now that we perfected
the indexing, another problem is upon us. How do we share and update
`EntityVec` from multiple threads? `Mutex` can save us from compiler errors
but it yields far from optimal performance for compiler sym table. We need to
get smarter and use some unsafe.

## Lock Free Strategy

All global language objects need to be available whenever the user imports them,
but they don't nescessary need to be writable. We also know that when we are
parsing and analyzing a file, no other thread will care about it (no cycles
between packages or modules). Though, we need to allow reallocation of the 
vectors.

From all of the mentioned requirements, I derived a strategy that requires
minimal amount of synchronization. At the core of all this is ref-counted
allocation. Instead of immediately deallocating old memory when storage needs
to reallocate, we let it potentially live by just decrementing ref count. This
means all threads can still view old allocation, but also means we have to
clone everything (will be fixed later).

This alone is not enough though, two threads cannot push to the same
allocation, nor read the pushed data immediately. We also want threads
to be able to modify some data when they compute it. This is where splitting
the storage comes in handy.

### Fragmentation

Lets, instead of one allocation, share a vector of allocations, each offering
special privileges to assigned thread. The privileged thread can extend the
allocation with new entities and modify them. Each allocation is split
into and region. Frozen region can be viewed by all threads
and modified by none. Remember, allocations are ref-counted, so more threads
can own it. The active region is only viewed by privileged thread. Thread can
extend this region, and also modify it.

Once the thread compiled all assigned code, it sets frozen boundary to active
boundary. After that, allocation is sent to other threads in case they depend
on latest compiled entities.

## Getting rid of cloning and arcs

I emntioned before, that we need to clone all entities in order to avoid double
free. This often means we need to arc the data that is expensive to clone. Or
do we?

There is not quite obvious but simple solution. When we reallocate the memory
we make it hold a ref-count to new allocation and when current ref-count goes
to zero and parent allocation is present, instead of calling destructors on the
accessible data, it will decrement parent (and also deallocating memory
region).

There is one problem though. If our memory contains interior mutability, user
can still produce double free. Here is an example code to demonstrate:

```rust
// there is no guarantee that allocation is the root
fn cause_double_free(allocation: &Alloc<Mutex<Vec<u8>>>) {
    // if this is not a root, we in fact dropped vector can still be
    // accessed by other threads as if it was still valid
    mem::take(*allocation[arbitrary].lock())
}
```

Luckly rust keeps all iterior mutability exposed in
[`UnsafeCell`](https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html)
and with some unstable features we can create a trait that will be implemented
only for types without it. Here is the code:

```rust
pub unsafe auto trait NoInteriorMutability {}

impl<T: ?Sized> !NoInteriorMutability for UnsafeCell<T> {}
```

Now we can guarantee stored data can't be accessed if it contains things
like Atomics and mutexes.

Lats thing we are missing is flexibility of `Arc<T>` by attaching Refcount
to raw pointer to an element inisde the allocation. This allows us to own
parts of data while also keeping memory safety (though one element can
keep whole allocation alive).

<!--
## Storing Language Constants

Up until now we vere storing just array like structures, implementing
constants that play well with metaprogramming features though is not so
straight forward.

What we need is an allocator that can manage arbitrarly complex nested
objects all serialized in flat memory, but also readable as a valid
datastrusture when needed. Thats quite something, but there actually is a way.

### Relocations

Trick to keep pointers valid even after reallocating the memory are relative
pointers. More precisely, a slite variation of them.

We can maintain object headers that will emember where the object is stored
and at which positions it contains relative pointers. Along the positions we
preserve relative offsets and there can be two types of these.

- **Local** pointer points within boundaries of one object.
- **Global** pointer points to data within foreign objects.

When the reallocation happens, we simply rewrite all pointers to corespond
to new location. Jit compiled code can then safely access constants and
perform computations on the data during compile time.

### Reallocating Memory While Preserving Alignment

So we now have an allocation full of objects with different alignment. How do
we reallocate such objects without messing up alignment?

As we allocate objects, we remember the one with biggest align and where it is
relative to allocation pointer. Then when we move the memory,
we simply look at destination pointer, add the offset and calculate desired
padding. We probably also want to remember where first allocated object starts
to save some extra bytes.

### TODO... 
--!>


