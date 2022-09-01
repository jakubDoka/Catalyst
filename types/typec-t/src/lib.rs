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
        #[derive(Copy, Clone)]
        pub enum $name {
            $(
                $kind$(($struct))?,
            )*
        }

        impl $name {
            pub fn try_cast<T: TryFrom<Self>>(self) -> Option<T> {
                self.try_into().ok()
            }

            pub fn cast<T: TryFrom<Self>>(self) -> T {
                self.try_cast().unwrap()
            }
        }

        $(
            $($(
                #[derive(Copy, Clone, Default)]
                pub struct $struct {
                    $(
                        pub $field: $ty,
                    )*
                }

                impl From<$struct> for $name {
                    fn from(value: $struct) -> Self {
                        $name::$kind(value)
                    }
                }

                impl TryInto<$struct> for $name {
                    type Error = ();
                    fn try_into(self) -> Result<$struct, Self::Error> {
                        match self {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }

                impl<'a> TryInto<&'a $struct> for &'a $name {
                    type Error = ();
                    fn try_into(self) -> Result<&'a $struct, Self::Error> {
                        match self {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }

                impl<'a> TryInto<&'a mut $struct> for &'a mut $name {
                    type Error = ();
                    fn try_into(self) -> Result<&'a mut $struct, Self::Error> {
                        match self {
                            $name::$kind(value) => Ok(value),
                            _ => Err(()),
                        }
                    }
                }
            )?)?
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
mod ty;
mod typec;

pub use {
    bound::{
        Bound, BoundBase, BoundExt, BoundFlags, BoundFunc, BoundFuncs, BoundInstance, BoundKind,
        BoundSlices, Bounds,
    },
    func::{Func, FuncFlags, FuncSlices, Funcs, Signature},
    ty::{
        Field, FieldFlags, Ty, TyExt, TyFlags, TyInstance, TyInteger, TyKind, TyPointer, TySlices,
        TyStruct, Types,
    },
    typec::{Flagged, Located, StorageExt, Typec, Variadic},
};

#[cfg(test)]
#[allow(dead_code)]
mod test {
    use super::*;

    #[derive(Clone, Copy)]
    pub struct Int;

    gen_kind!(TestKind
        A = TestA {
            a: u32,
            b: u32,
        },
        C = Int,
    );
}
