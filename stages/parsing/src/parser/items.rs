use super::*;

impl Parser<'_> {
    pub fn parse_items(&mut self) -> (VSlice<Ast>, bool) {
        self.parse_with(Self::take_items)
    }

    pub fn parse_items_spanned(&mut self) -> (VSlice<Ast>, Span, bool) {
        let start = self.state.last_break.unwrap_or(0);
        let (items, finished) = self.parse_items();
        let end = self
            .state
            .last_break
            .unwrap_or(self.state.current.span.end());
        (items, Span::new(start..end), finished)
    }

    pub fn take_items(&mut self) -> errors::Result {
        self.skip_newlines();
        self.list(
            None,
            TokenKind::NewLine,
            [TokenKind::Break, TokenKind::Eof],
            Self::item,
        )
        .map(|span| self.state.last_break = Some(span.end()))
    }

    pub fn item(&mut self) -> errors::Result {
        branch! { self => {
            Func => self.r#fn()?,
            Struct => self.r#struct()?,
            Bound => self.bound()?,
            Impl => self.r#impl()?,
        }};
        Ok(())
    }
}
