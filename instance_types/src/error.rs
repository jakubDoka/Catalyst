use lexer_types::*;

pub enum InstError {
    InvalidBitCast {
        loc: Span,
        instantiated_from: Option<Span>,
        from: String,
        from_size: usize,
        to: String,
        to_size: usize,
    },
}