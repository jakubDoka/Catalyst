# MetaMach

## Compilation model

Stages of compilation:

1. build dependency tree
    * parse manifests
    * download needed dependency
    * report cycles
2. compile each dependency in parallel
    * build module tree
    * report cycles
    * for each module
        * build AST
            * expand token macros
            * expand AST macros
        * generate type IR
            * expand type macros 
        * generate concrete IR
            * expand concrete macros
        * generate Cranelift IR
            * inline
        * compile macros
    * compile Cranelift IR
        * optimize
        * generate native code
3. collect all functions into binary
