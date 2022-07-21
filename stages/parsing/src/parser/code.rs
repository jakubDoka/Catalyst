use super::*;

impl Parser<'_> {
    pub fn parse_code(&mut self) -> (Maybe<AstList>, bool) {
        self.parse_with(Self::take_code)
    }

    pub fn take_code(&mut self) -> errors::Result {
        loop {
            branch! { self => {
                Struct => self.r#struct()?,
                NewLine => self.skip_newlines(),
                Eof => break,
                Break => break,
            }}
        }

        Ok(())
    }
}
