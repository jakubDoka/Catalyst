# Type checking

Type checking is a challenge, mainly because of how powerful I want it to be. What I see as powerful is having:
  - `generics` - Generics are useful in reducing code duplication and every strongly typed language should have some form of generics (IMO).
  - `specifications` - Since we have generics, we also want to declare what they can do in type safe manner.
  - `inference` - Type inference makes working with strong type system pleasant and code is overall more adaptable type changes. To boost the inference, we want associated types.

In order to do this though, we need logical proofs to verify whether type complies with specification. How to design such thing?

## Predicate logic

If we model our `specification system`(SS) our lives can be greatly simplified. General syntax of logical languages boils down to this:

```rust
// statement proves that impl of type and trait is always true, thus is root fact, we can write
// this as `impl Trait for Type` in our case.
(impl type trait) :- ()
// We of course want more complex proofs that include conditions. For example we write
// `impl [T] Trait for Seq[T] where T: Trait`
(impl (seq T) trait) :- (impl T trait)
```

This model is great, only problem occurs when we try to add associate types into the mix. Lets break down the behavior we expect from applying conditions on associate types. We will go case by case.

```rust
trait Clone {}
trait Trait {
  type Asoc: Clone;
}

struct Type;
struct Seq<T>;

// Type is concrete so we check predicates against it.
impl Trait for Type {
  type Asoc = Seq<Type>;
}

// Type is generic so predicates are attached to it. This should be done exhaustively
// for all generic parameters and associate types.
impl<T> Trait for Seq<T> {
  type Asoc = T;
}

trait Copy where Self: Clone {}
trait Extension<T> where
  Self: Trait,
  Self::Asoc: Copy,
  T: Copy {}

// Now this gets more complex. `T` is implied to be `Copy` which implies it is `Clone`.
// It checks if `Seq<T>` implements `Trait` and from that is also implies `T` is `Clone`
// again. Predicate on `Asoc` also adds `Copy` again.
impl<T> Extension<T> for Seq<T> {}

// Since this is a concrete type, we just check all predicates and nothing is implied.
impl Copy for Type {}
impl Extension<Type> for Type {}
```

With this little brainstorm, we can start to sketch the algorithm. After small thought, we will probably need two passes.
  1. find all implied bounds and push them to proofs temporarily.
  2. check all predicates.

Well that is simple then I thought and i think we can do this very quickly.

## Instancing Garbage

The algorithm used in its nature puts pressure on global type interner. We create numerous temporary instances that are used in intermediate steps we dont care about after proving a bound. This calls for another premature optimization, temporary instances. Type should have its active and passive format, we can find passive types in all persistent objects. Upon performing computations on these types, we convert them to the active form, that is easier to manipulat but takes more memory. When loading to memory, we generally want to keep them for shortes abount possible which can be achived with arena scopes. This also grants us ability to make passive types as compact as posssible.
