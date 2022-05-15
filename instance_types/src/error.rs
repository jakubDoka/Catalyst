use lexer_types::*;

use crate::Layout;

pub enum InstError {
    InvalidBitCast {
        loc: Span,
        instantiated_from: Option<Span>,
        from: String,
        from_layout: Layout,
        to: String,
        to_layout: Layout,
    },
}
