use parser::AnyError;

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    InvalidCallConv,
}
