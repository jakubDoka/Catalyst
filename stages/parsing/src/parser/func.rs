use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn func_def(&mut self, vis: Option<VisAst<M>>) -> Option<FuncDefAst<'arena, M>> {
        Some(FuncDefAst {
            vis,
            signature: self.func_sig()?,
            body: self.func_body()?,
        })
    }

    pub fn func_sig(&mut self) -> Option<FuncSigAst<'arena, M>> {
        Some(FuncSigAst {
            r#fn: self.advance(),
            cc: self.try_advance(Tk::Str),
            generics: self.generics()?,
            name: self.name("function")?,
            args: self.opt_tuple("function arguments", Self::func_arg)?,
            ret: self
                .try_advance(Tk::RightArrow)
                .map(|arrow| self.ty().map(|ty| (arrow, ty)))
                .transpose()?,
        })
    }

    fn func_arg(&mut self) -> Option<FuncArgAst<'arena, M>> {
        Some(FuncArgAst {
            pat: self.pat(None)?,
            colon: self.expect(Tk::Colon, |ctx| MissingColon {
                something: "function argument",
                found: ctx.state.current.kind,
                loc: ctx.loc(),
            })?,
            ty: self.ty()?,
        })
    }

    fn func_body(&mut self) -> Option<FuncBodyAst<'arena, M>> {
        branch! {self => {
            ThickRightArrow => {
                let arrow = self.advance();
                self.skip(TokenKind::NewLine);
                self.expr().map(|e| FuncBodyAst::Arrow(arrow, e))
            },
            LeftBrace => self.object("function body", Self::expr).map(FuncBodyAst::Block),
            Extern => Some(FuncBodyAst::Extern(self.advance())),
            @"function body",
        }}
    }
}
