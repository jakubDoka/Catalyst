# Catalyst

General purpose Programming language, focused on metaprogramming (not quite
threre yet). Language is designed with simple syntax that does not face any
parsing edge cases. Catalist tries to offer similar to same features as
[Rust](https://www.rust-lang.org/) and [Nim](https://nim-lang.org/)
combined. Language is low level, powered by
[Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
backend. Compiler is highly paralelized and incemental, I in general consider
performance for better or worse.

## Quick-Start

### Requirements

Either of:
- [gcc](https://gcc.gnu.org/)
- [clang](https://clang.llvm.org/)
- [vs tools](https://visualstudio.microsoft.com/downloads/)
- [mold](https://github.com/rui314/mold) - fastest

All of:
- [Rust](https://www.rust-lang.org/tools/install)
- [Git](https://git-scm.com/downloads)

### Support

Tested on:
- Arch Linux

Untested but should work:
- Windows - maybe
- Mac - unlikely
- Linux - likely

### Installation

```bash
git clone --depth 1 http://gighub.com/jakubDoka/Catalyst
cd Catalyst
cargo install --path .
```

### Trouble-Shooting

TODO

## Why should I try Catalyst?

Its a baby language that needs some stress testing. By that I mean I need
someone who does not know how language works internally, to use the language.
Tests that are performed are more of a sanity checks that verify key features
work but in general dont test for correct error messages.

To put it simply, using Catalyst is charitative deed, not nesscesarly something
you would do to be produdctive yet.

## Why should I contribute to Catalyst?

If you tryed the language, and feel like some feature is missing or you are
annoyed by some bug, and waiting for me to fix it is boring, you can take things
into your own hands and make a pull request. Note though that pull request needs
issue first.

Typos are categorized under bugs you can fix. (I do a lot of them :])
