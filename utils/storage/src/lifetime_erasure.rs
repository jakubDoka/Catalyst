use std::{
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
};

#[macro_export]
macro_rules! gen_erasable {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident<$($lt:lifetime),*> {
            $($fvis:vis $field:ident: $ty:ty),* $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name<$($lt),*> {
            $($fvis $field: $ty),*
        }

        unsafe impl<$($lt),*> Erasable for $name<$($lt),*> {
            type Erased = $crate::gen_erasable!(@inner $name $('static $lt)*);

            fn clear(&mut self) {
                $(self.$field.clear();)*
            }
        }
    };

    (@inner $name:ident $($lt:lifetime $ignored:lifetime)*) => {
        $name<$($lt),*>
    };
}

/// Trait should not be implemented manually. Use `gen_erasable!` if possible.
pub unsafe trait Erasable {
    type Erased: 'static;

    fn clear(&mut self);
}

#[repr(transparent)]
pub struct Active<T>(T);

impl<T: Erasable> Active<T> {
    pub fn new(e: T::Erased) -> Self {
        const {
            assert!(std::mem::size_of::<T>() == std::mem::size_of::<T::Erased>());
            assert!(std::mem::align_of::<T>() == std::mem::align_of::<T::Erased>());
        }

        // SAFETY: The erased type is guaranteed to be the same size and alignment as the active type.
        // Whether the cast is safe depends on wheter erasable can be safely casted to erased.
        unsafe {
            let e = ManuallyDrop::new(e);
            Self(std::ptr::read(e.deref() as *const _ as *const T))
        }
    }

    pub fn erase(mut self) -> T::Erased {
        const {
            assert!(std::mem::size_of::<T>() == std::mem::size_of::<T::Erased>());
            assert!(std::mem::align_of::<T>() == std::mem::align_of::<T::Erased>());
        }

        self.0.clear();

        unsafe {
            let mut e = ManuallyDrop::new(self.0);
            std::ptr::read(e.deref_mut() as *mut _ as *mut T::Erased)
        }
    }
}

impl<T> Deref for Active<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Active<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
