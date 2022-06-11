use lexer::Span;

pub enum OwError {
    DoubleMove { because: Span, loc: Span },
    MoveFromBehindPointer { loc: Span },
    LoopDoubleMove { because: Span, loc: Span },
    PartiallyMovedDrop { because: Span, loc: Span },
}
