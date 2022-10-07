#![feature(default_free_fn)]
#![allow(incomplete_features)]
#![feature(specialization)]
#![feature(option_result_contains)]
#![feature(iter_intersperse)]
#![feature(result_flattening)]
#![feature(let_else)]
#![feature(if_let_guard)]

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
        #[derive(Copy, Clone, PartialEq, Eq, Debug)]
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
                self.try_cast().unwrap_or_else(|| unreachable!("invalid cast {:?}", self))
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
                    #[derive(Copy, Clone, Default, PartialEq, Eq, Debug)]
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

mod func;
mod scope;
mod tir;
mod ty;
mod typec;

pub use {
    func::{Func, FuncFlags, FuncSlices, FuncVisibility, Funcs, Signature},
    graphs::ProjectedCycleDetector as TyGraph,
    scope::{ModuleItem, Scope, ScopeError, ScopeFrame, ScopeItem, ScopeRecord},
    tir::{
        AccessTir, BlockTir, CallTir, CallableTir, ConstTir, ConstructorTir, DerefTir, IntLit,
        IntTir, NodeInput, RefTir, ReturnTir, TirBuilder, TirFrame, TirNode, TypecOutput,
        TypedTirNode, Var, VarTir, Variable,
    },
    ty::{
        Field, FieldFlags, Fields, Impl, ImplLookup, Impls, SpecFunc, SpecFuncs, Ty, TyExt,
        TyFlags, TyInstance, TyInteger, TyKind, TyPointer, TySlices, TySpec, TyStruct, Types,
    },
    typec::{Loc, Located, Typec},
};
