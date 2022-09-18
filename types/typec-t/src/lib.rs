#![feature(default_free_fn)]

use std::convert::TryInto;

#[macro_export]
macro_rules! gen_kind {
    (
        $name:ident
            $(
                $kind:ident$( = $struct:ident $({
                    $($field:ident: $ty:ty,)*
                })?)?,
            )*
    ) => {
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub enum $name {
            $(
                $kind$(($struct))?,
            )*
        }

        impl $name {
            pub fn try_cast<T>(self) -> Option<T> where Self: TryInto<T> {
                self.try_into().ok()
            }

            pub fn try_cast_mut<'a, T>(&'a mut self) -> Option<&'a mut T>
            where
                &'a mut Self: TryInto<&'a mut T> {
                self.try_into().ok()
            }

            pub fn try_cast_ref<'a, T>(&'a self) -> Option<&'a T>
            where
                &'a Self: TryInto<&'a T> {
                self.try_into().ok()
            }

            pub fn cast<T>(self) -> T where Self: TryInto<T> {
                self.try_cast().unwrap()
            }

            pub fn cast_ref<'a, T>(&'a self) -> &'a T
            where
                &'a Self: TryInto<&'a T> {
                self.try_cast_ref().unwrap()
            }

            pub fn cast_mut<'a, T>(&'a mut self) -> &'a mut T
            where
                &'a mut Self: TryInto<&'a mut T> {
                self.try_cast_mut().unwrap()
            }
        }

        $(
            $(
                $(
                    #[derive(Copy, Clone, Default, PartialEq, Eq)]
                    pub struct $struct {
                        $(
                            pub $field: $ty,
                        )*
                    }
                )?

                impl From<$struct> for $name {
                    fn from(value: $struct) -> Self {
                        $name::$kind(value)
                    }
                }

                impl TryInto<$struct> for $name {
                    type Error = ();
                    fn try_into(self) -> Result<$struct, Self::Error> {
                        #[allow(unreachable_patterns)]
                        match self {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }

                impl<'a> TryInto<&'a $struct> for &'a $name {
                    type Error = ();
                    fn try_into(self) -> Result<&'a $struct, Self::Error> {
                        #[allow(unreachable_patterns)]
                        match self {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }

                impl<'a> TryInto<&'a mut $struct> for &'a mut $name {
                    type Error = ();
                    fn try_into(self) -> Result<&'a mut $struct, Self::Error> {
                        #[allow(unreachable_patterns)]
                        match self {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }
            )?
        )*
    };
}

#[macro_export]
macro_rules! impl_located {
    ($ty:ty) => {
        impl $crate::Located for $ty {
            #[inline]
            fn loc(&self) -> Loc {
                self.loc
            }
        }
    };
}

#[macro_export]
macro_rules! impl_variadic {
    ($ty:ty, $kind:ty) => {
        impl $crate::Variadic for $ty {
            type Kind = $kind;
            fn kind(&self) -> &Self::Kind {
                &self.kind
            }
            fn kind_mut(&mut self) -> &mut Self::Kind {
                &mut self.kind
            }
        }
    };
}

#[macro_export]
macro_rules! impl_flagged {
    ($ty:ty, $flags:ty) => {
        impl $crate::Flagged for $ty {
            type Flags = $flags;
            fn flags(&self) -> &Self::Flags {
                &self.flags
            }
            fn flags_mut(&mut self) -> &mut Self::Flags {
                &mut self.flags
            }
        }
    };
}

mod bound;
mod func;
mod tir;
mod ty;
mod typec;

pub use {
    bound::{
        Bound, BoundBase, BoundExt, BoundFlags, BoundFunc, BoundFuncs, BoundInstance, BoundKind,
        BoundSlices, Bounds,
    },
    func::{Func, FuncFlags, FuncSlices, Funcs, Signature},
    tir::{
        Block, BlockInputTir, BlockTir, BodyTir, BranchTir, CallTir, ControlFlowTir, FuncTir,
        InstTir, JumpTir, TirBuilder, TirBuilderCtx, ValueTir, Var,
    },
    ty::{
        Field, FieldFlags, Fields, Ty, TyExt, TyFlags, TyInstance, TyInteger, TyKind, TyPointer,
        TySlices, TyStruct, Types,
    },
    typec::{Flagged, Located, StorageExt, Typec, Variadic},
};

#[cfg(test)]
#[allow(dead_code)]
mod test {
    use super::*;

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Int;

    gen_kind!(TestKind
        A = TestA {
            a: u32,
            b: u32,
        },
        C = Int,
    );
}
