import sys
AMOUNT = int(sys.argv[1])

file = open("bench_project/root.mf", "w")
file.write(f"""
    #entry
    fn main() -> int {{ 
        gathering()
    }}

    bound B {{
        fn new() -> Self
        fn foo(s: *Self) -> int
        fn bar(s: *Self) -> int
    }}

    fn [T: B] great() -> int {{
        let s = T::new();
        s.foo() + s.bar()
    }}

    fn gathering() -> int {{
        let mut sum = 0;
        {";".join((f"sum += great::[S{i*i}]()") for i in range(AMOUNT))}
        sum
    }}
""".replace(";" , "\n"))
for i in range(AMOUNT):
    i *= i
    file.write(f"""
        struct S{i} {{
            data: int
        }}

        impl B as S{i} {{
            fn new() -> Self {{
                S{i}::{{
                    data: {i}
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