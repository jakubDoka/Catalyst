use std::default::default;

use crate::*;

impl BuiltinBuilder<'_> {
    pub fn build(&mut self) {
        self.make_builtin_types();
        self.make_builtin_operators();
    }

    fn make_builtin_operators(&mut self) {}

    fn make_builtin_types(&mut self) {
        macro_rules! gen_types {
            ($($($ty:ident)* => $ent:expr,)*) => {
                gen_types_recur!((0) $($($ty => $ent),*),*);
            };
        }

        macro_rules! gen_types_recur {
            (($init:expr) $ty:ident => $ent:expr $(, $($else:tt)*)?) => {
                const _: () = assert!(Ty::$ty.index() == $init);
                self.add_type(stringify!($ty), $ent);
                $( gen_types_recur!(($init + 1) $($else)*); )?
            };
        }

        macro_rules! int {
            ($width:literal $signed:literal) => {
                Ty {
                    kind: TyInt {
                        width: $width,
                        signed: $signed,
                    }
                    .into(),
                    ..default()
                }
            };
        }

        gen_types! {
            INFERRED => default(),
            SELF_BOUND => default(),
            STR STACK_TRACE => default(), // TODO
            BOOL => Ty {
                kind: TyKind::Bool,
                ..default()
            },
            CHAR => int!(32 false),
            INT => int!(0 true),
            I8 => int!(8 true),
            I16 => int!(16 true),
            I32 => int!(32 true),
            I64 => int!(64 true),
            UINT => int!(0 false),
            U8 => int!(8 false),
            U16 => int!(16 false),
            U32 => int!(32 false),
            U64 => int!(64 false),
        }
    }

    fn add_type(&mut self, name: &str, mut ent: Ty) {
        let id = self.interner.intern_str(&name.to_ascii_lowercase());
        ent.loc.name = id;
        self.typec.types.insert_unique(id, ent);
    }
}
