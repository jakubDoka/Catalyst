pub trait Invalid: Sized {
    fn invalid() -> Self;

    fn is_invalid(&self) -> bool where Self: PartialEq {
        *self == Self::invalid()
    }
}

pub struct Maybe<T>(T);