# Stages

Compilation is split into stages (crates) with minimal dependencies. Each stage
tries to abstract single transfromarion. Currently stages are as follows:

- [lexing](lexing.md)
- [parsing](parsing.md)
- [type checking](typec.md)
- [borrow checking](borrowc.md)
- [codegen](gen.md)

The general compiler design is followed, crates also only contain logic and
stage specific types, so stages don't import each other with exception of parsing
being dependant on lexing since tokens are consumed lazily.
