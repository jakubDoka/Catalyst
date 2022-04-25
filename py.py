file = open("bench_project/root.mf", "w")
file.write("fn main() -> int { return 0 }")
for i in range(10000):
    file.write(f"""
    fn other{i}(a: int, b: int, c: int) -> int {{
        return a + b * c
    }}

    fn bti{i}(b: bool) -> int {{
        return if b {{ 1 }} else {{ 0 }}
    }}

    fn structure_test{i}() -> int {{
        let mut something = init_something{i}()
        something = Something{i}::{{
            a: 2
            b {{
                e: true
                f: false
            }}
            e: true
        }}

        let something_else = init_something_else{i}()

        if something.e {{}} else {{
            return -1
        }}

        something.a - bti{i}(something.b.e) + -bti{i}(something_else.f)
    }}

    fn init_something_else{i}() -> SomethingElse{i} {{
        SomethingElse{i}::{{ e: false; f: true }}
    }}

    fn init_something{i}() -> Something{i} {{
        Something{i}::{{
            a: 0
            b {{
                e: false
                f: false           
            }}
            e: false
        }}
    }}

    struct Something{i} {{
        a: int
        b: SomethingElse{i}
        e: bool
    }}

    struct SomethingElse{i} {{
        e: bool
        f: bool
    }}
    """)