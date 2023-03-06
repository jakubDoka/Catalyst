# Codegen

## Interpreter and Jit

Problem at hand is how exactly do we combine the two. How to efficiently determine
when to jit compile to speed up compile time. How do we call jitted functions from
interpreter and how do we manage jitting in first place.

### Organization

This part should not be a problem. We already inject interpreter into typec. We
can further inject codegen into interpreter. Injection can happen trough
`&mut dyn Trait` which in terms needs one method method `jit` which takes a context
and outputs function handle with lifetime bound to the object.

### Calling

All jitted functions have no exposed API to compiler, instead they are called
trough adapters that take two pointers, function and arguments. Adapter will then
call the function natively, unpacking passed arguments in continuous memory.

### When to jit something?

Most importantly, this cannot be determined by an exact algorithm. Rather we will
use heurictic with tweekable parameters. First paramteter and also the simplest
to implement is how much the function takes to execute in total. This reflects
both slowness of the function and how often it gets called. If this parameter
exceeds a threshold, we jit it. This may go horribly or perfectly well but in general,
it should perform better then not jitting.

User can also help compiler identify hot code, which can improve performance
tremendously for costly functions runned only once.

Error can be also added that forces user to add `#[jit]` to a function manually to
maintain good build performance.
