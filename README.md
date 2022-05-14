# Catalyst

## Reasoning

I decided to make a compiler because I just love challenges. To be precise, making a programming language just gives me a lot of dopamine. You may say I am wasting my time but form me this is gaining experience. Harder the goal is the more you improve while achieving it.

## Main Goals

I am trying to produce a language that compiles quickly, helps you as much as possible and allows you to do anything. I love when I can constantly discover new things and improve my productivity by gathering more experience with the tool. My biggest inspiration is `Rust` and `Nim`. `Rust` brings amazing safety and confidence in your code and `Nim` empowers you with powerful macro system. I am in deed a metaprogramming freak and adore Domain Specific Languages (DSL). 

So the main features you can look out for are:
- `macros`: Any code can be called from macro and writing performant macro code is Important. Idea of complex and easy to write macros came from `Nim`. I want to experiment with `Catalyst` and see what kind of macro model is the best. So macros will have ability to connect to compiler pipeline at any point and modify the behavior. Compilation stages are `String -> Token -> Ast -> Tir -> Mir -> Cir` and macro can create a bridge between two successive stages.
- `generics`: Safe generic system based of `bound`s, which are very similar to `Rust`'s `trait`s.
- `scope based methods`: You can define methods on any type, name collisions are resolved by scoping.
- `type inference`: Types are inferred to ease the use of language and make code easier to refactor.
- `safety`: Compiler will bother you if you write something questionable. Just like `Rust`.
- `no garbage collector`: Lifetime of data is determined at compile time. `Rust` at it again.
- `speedy compilation`: Compiler is written in memory and cache efficient way, also using `Cranelift` that is optimized for best compile-time/run-time performance ratio.
- `incremental builds`: Project should scale and use as much code as needed while preserving quick development cycle.
- `debug != release`: You can choose between `debug` and `release` mode to prioritize compile-time or run-time respectively.

**Note:** `Catalyst` is a work in progress and features you've seen above are more planned than implemented.

## TODO

- [ ] Language constructs
    - [ ] Functions
        - [ ] Linkage
        - [x] Call convention
        - [ ] Generics
            - [x] Bounds
        - [x] Methods
        - [x] Procedures
        - [ ] Statements
            - [x] Return
            - [x] Break - also as top level code segmentation
            - [ ] Continue - only loop
            - [x] Variables
            - [ ] Expressions
                - [x] Control Flow
                    - [x] If-Elif-Else
                    - [x] Loop
                - [x] Binary
                - [x] Unary
                - [ ] Call
                    - [x] Procedural
                    - [ ] Object-Oriented
                - [ ] Constants
                    - [x] Boolean
                    - [ ] Folding
                    - [x] Integer
                    - [ ] Float
                    - [x] Structures
                    - [ ] Arrays
    - [ ] Macros
        - [ ] Jit
        - [ ] String
        - [ ] Token
        - [ ] Ast
        - [ ] Tir
        - [ ] Mir
        - [ ] Ir
    - [ ] Pointers
        - [ ] Mutable
        - [ ] Immutable
    - [ ] References
        - [ ] Mutable
        - [ ] Immutable
    - [ ] Structures
        - [ ] Generics
        - [ ] Safe tagged unions
        - [x] Structs
        - [ ] Bounds
    - [ ] Visibility
        - [ ] Package Export
        - [x] Package Public
        - [ ] File Private

- [ ] Diagnostics
    - [ ] Errors
    - [x] Recovery
    - [ ] Warnings