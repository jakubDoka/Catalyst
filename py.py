import sys
AMOUNT = int(sys.argv[1])
RATIO = int(sys.argv[2])

split = AMOUNT // RATIO

modules = ['root/mod' + str(i) for i in range(split)]

imports = ";".join([f'"{i}"' for i in modules])

file = open("bench_project/root.mf", "w")
file.write(f"""
    use {{
        "root/bound_def"
        {imports}
    }}

    #entry
    fn main() -> int {{ 
        gathering()
    }}

    fn [T: B] great() -> int {{
        let s = T::new();
        s.foo() + s.bar()
    }}

    fn gathering() -> int {{
        let mut sum = 0;
        {";".join((f"sum += great::[S{i}]()") for i in range(0, AMOUNT, 100))}
        sum
    }}
""".replace(";" , "\n"))

import os

for (i, m) in enumerate(modules):
    path = "bench_project/" + m + ".mf"
    file = open(path, "w+")
    file.write(f"""
        use {{
            "root/bound_def"
        }}
    """)   
    for j in range(RATIO):
        id = i * RATIO + j
        file.write(f"""
            struct S{id} {{
                data: int
            }}

            fn smh{id}(n: int) -> int {{
                let mut n = n
                let a = loop {{
                    if n == 0 {{
                        break 0
                    }} else {{
                        n -= 1
                    }}
                }}

                if n != 0 {{
                    return -1
                }} else if n == 0 {{
                    smh{id}(smh{id}(smh{id}(n)))
                }}

                return a
            }}

            impl B as S{id} {{
                fn new() -> Self {{
                    S{id}::{{
                        data: {id}
                    }}
                }}

                fn foo(s: *Self) -> int {{
                    s.data
                }}

                fn bar(s: *Self) -> int {{
                    s.data * 2
                }}
            }}
        """)