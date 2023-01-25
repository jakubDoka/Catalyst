# Code Structure

Organizing code becomes increasingly important as the ptoject grows. This
applies to Rust even more then other languages as splitting code into
separate crates can improve compile times and improve development cycle.

Tis chapter will walk trough evolution of Catalist's code strusture.

## Single Crate

Beginner mistake (I made) is putting everithing into singular crate and
organizing compilation stages into modules. At first this may look
logical, and it is in fact logical in languages like Go, where cyclic
dependencies are not allowed. With no cycles between modules, compilers
can optimize memory usage and paralelize compilation. Not to mention
incremental compilation.

By putting all your code into single crate, you prevent rust compiler
from doing any of the mentioned optimizations. Other significant problem
comes with modularity of the code. Making the your code scale mode
horizontaly then vertically is in general a great idea. What i mean is:

 Vertical                  | Horizontal
:-------------------------:|:-------------------------:
![](./tall-tree.jpeg)      |  ![](./wide-tree.jpg)

## Sequential Crates

Let's say yo scale vertically. Your graph can look like this:

```
  Compiler
    Codegen
      Typecheck
        Parsing
          Lexing
```

Graph is deep and only have one leaf node. This means taht if you make
change to Lexing, everithing needs to recompile. Notice that Typecheckig
is actually not consorned with how parser parses, it only cares about final
datastructure it outputs. Yes you can in fact invoke `Parsing` from
`Typechecking` but there is only **data relation** when you solve the
typechecking it self. Similar goes for `Codegen -> Typecheck` relation.

We can do better with bit more crates.

## Data and Behavior Crates

```
  Compiler
    Codegen
      Types
    Typecheck
      Ast
      Types
    Parsing
      Ast
      Lexing
    Ast
    Types
```

Separating **shared** type definitions from behavior allows us to get rid
of two levels of nesting. Now if we change something in lexing, only `Parsing`
and `Compiler` is recompiled. The trick is to write all the glue code in
compiler. Compiler can use `Parsing` to make `Ast`, transform `Ast` into
`Types` IR and finaly translate it to binary with `Codegen`.

Sharing as little as possible between code with different focus comes in
handy when refactoring. Total rewrite of Parser will only affect `Typecheck`
and `Compiler` since we cannot access any parsing related code in `Codegen`
and `Types`.

## Conclusion

~~Good~~ My preferred code structure is modular with as little connections as
possible. Its probably the only OOP concept that is actually usefull
(encapsulation). The measurable benefits are shorter compile times, easier
reasoning about each component, and faster refactoring.
