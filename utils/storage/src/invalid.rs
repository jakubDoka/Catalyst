use serde::{Deserialize, Serialize};

/// Introduces the concept of invalid value for the implementor type.
pub trait Invalid: Sized {
    /// invalid value constructor, it is unsafe
    /// to prevent accidental misuse.
    /// # Safety
    /// Its only safe to use returned value in [`Maybe`] struct.
    unsafe fn invalid() -> Self;

    /// checks if the value is invalid. This should be
    /// cheaper then 1:1 comparison if possible.
    fn is_invalid(&self) -> bool;
}

/// Maybe is very similar to [`Option`] but it keeps `sizeof Maybe<T> == sizeof T`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Maybe<T>(T);

impl<T: Invalid> Maybe<T> {
    /// Creates a new `Maybe` containing valid value.
    ///
    /// # Panics
    ///
    /// Panics if the value is invalid.
    pub const fn some(value: T) -> Self {
        Maybe(value)
    }

    /// Creates a new `Maybe` containing invalid value.
    pub fn none() -> Self {
        Maybe(unsafe { T::invalid() })
    }

    /// Grants optional mutable access.
    pub fn as_mut_option(&mut self) -> Option<&mut T> {
        if self.0.is_invalid() {
            None
        } else {
            Some(&mut self.0)
        }
    }

    /// Grants optional immutable access.
    pub fn as_ref_option(&self) -> Option<&T> {
        if self.0.is_invalid() {
            None.take()
        } else {
            Some(&self.0)
        }
    }

    /// Takes the value out of the maybe, leaving an invalid value in its place.
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Self::none())
    }

    /// Transforms [`Maybe`] into [`Option`].
    pub fn expand(self) -> Option<T> {
        if self.0.is_invalid() {
            None
        } else {
            Some(self.0)
        }
    }

    /// Returns true if value is valid.
    pub fn is_some(&self) -> bool {
        !self.0.is_invalid()
    }

    /// Returns true if value is invalid.
    pub fn is_none(&self) -> bool {
        self.0.is_invalid()
    }

    /// Returns the contained value of panics.
    pub fn unwrap(self) -> T {
        assert!(self.is_some());
        self.0
    }
}

impl<T: Invalid> From<Option<T>> for Maybe<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => Maybe(value),
            None => Maybe::none(),
        }
    }
}

impl<T: Invalid> From<T> for Maybe<T> {
    fn from(value: T) -> Self {
        Maybe::some(value)
    }
}

impl<T: Invalid> Default for Maybe<T> {
    fn default() -> Self {
        Maybe::none()
    }
}
