use serde::{Deserialize, Serialize};

pub trait Invalid: Sized {
    unsafe fn invalid() -> Self;

    fn is_invalid(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Maybe<T>(T);

impl<T: Invalid> Maybe<T> {
    pub fn some(value: T) -> Self {
        Maybe(value)
    }

    pub fn none() -> Self {
        Maybe(unsafe { T::invalid() })
    }

    pub fn as_mut_option(&mut self) -> Option<&mut T> {
        if self.0.is_invalid() {
            None
        } else {
            Some(&mut self.0)
        }
    }

    pub fn take(&mut self) -> Self {
        if self.0.is_invalid() {
            Self::none()
        } else {
            std::mem::replace(self, Self::none())
        }
    }

    pub fn expand(self) -> Option<T> {
        if self.0.is_invalid() {
            None
        } else {
            Some(self.0)
        }
    }

    pub fn is_some(&self) -> bool {
        !self.0.is_invalid()
    }

    pub fn is_none(&self) -> bool {
        self.0.is_invalid()
    }

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
