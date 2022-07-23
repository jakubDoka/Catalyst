use super::*;

impl Parser<'_> {
    pub fn parse_items(&mut self) -> (Maybe<AstList>, bool) {
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
    }

    pub fn item(&mut self) -> errors::Result {
        branch! { self => {
            Fn => self.r#fn()?,
            Struct => self.r#struct()?,
        }};
        Ok(())
    }
}
