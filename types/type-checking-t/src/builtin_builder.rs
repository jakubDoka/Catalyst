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
                const _: () = assert!(BuiltinTypes::$ty.id() == $init);
                self.add_type(stringify!($ty), $ent);
                $( gen_types_recur!(($init + 1) $($else)*); )?
            };
        }

        macro_rules! int {
            ($width:literal $signed:literal) => {
                TyEnt {
                    kind: TyKind::Int {
                        width: $width,
                        signed: $signed,
                    },
                    ..default()
                }
            };
        }

        gen_types! {
            INFERRED => default(),
            DROP COPY => TyEnt {
                flags: TyFlags::GENERIC,
                ..default() // TODO
            },
            STR STACK_TRACE => default(), // TODO
            TY_ANY => TyEnt {
                flags: TyFlags::GENERIC | TyFlags::TY_PARAM,
                kind: TyKind::default_param(),
                ..default()
            },
            ANY => TyEnt {
                flags: TyFlags::GENERIC,
                kind: TyKind::default_bound(),
                ..default()
            },
            BOOL => TyEnt {
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

    fn add_type(&mut self, name: &str, ent: TyEnt) {
        let id = self.interner.intern_str(&name.to_ascii_lowercase());
        self.typec.types.insert(id, ent);
    }
}
