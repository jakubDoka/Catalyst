use super::*;

impl Parser<'_> {
    pub fn parse_items(&mut self) -> (VSlice<Ast>, bool) {
        self.parse_with(Self::take_items)
    }

    pub fn take_items(&mut self) -> errors::Result {
        self.skip_newlines();
        self.list(
            None,
            TokenKind::NewLine,
            [TokenKind::Break, TokenKind::Eof],
            Self::item,
        )
        .map(|_| ())
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
