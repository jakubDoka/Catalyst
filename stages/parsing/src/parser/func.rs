use scope::Vis;

use super::*;

list_meta!(FuncArgMeta ? LeftParen Comma RightParen);
pub type FuncArgsAst<'a> = ListAst<'a, FuncArgAst<'a>, FuncArgMeta>;

#[derive(Clone, Copy, Debug)]
pub struct FuncDefAst<'a> {
    pub vis: Vis,
    pub signature: FuncSigAst<'a>,
    pub body: FuncBodyAst<'a>,
    pub span: Span,
}

impl<'a> Ast<'a> for FuncDefAst<'a> {
    type Args = (Vis, Span);

    const NAME: &'static str = "function definition";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (vis, start): Self::Args) -> Option<Self> {
        let signature = ctx.parse()?;
        let body = ctx.parse::<FuncBodyAst>()?;
        let span = start.joined(body.span());

        Some(Self {
            vis,
            signature,
            body,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncSigAst<'a> {
    pub fn_span: Span,
    pub cc: Option<NameAst>,
    pub generics: GenericsAst<'a>,
    pub name: NameAst,
    pub args: FuncArgsAst<'a>,
    pub ret: Option<TyAst<'a>>,
}

impl<'a> Ast<'a> for FuncSigAst<'a> {
    type Args = ();

    const NAME: &'static str = "function signature";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        Some(Self {
            fn_span: ctx.advance().span,
            cc: ctx
                .try_advance(TokenKind::String)
                .map(|tok| NameAst::new(ctx, tok.span.shrink(1))),
            generics: ctx.parse()?,
            name: ctx.parse()?,
            args: ctx.parse()?,
            ret: ctx
                .try_advance(TokenKind::RightArrow)
                .and_then(|_| ctx.parse()),
        })
    }

    fn span(&self) -> Span {
        self.generics
            .span()
            .joined(self.ret.map_or(self.args.span(), |r| r.span()))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncArgAst<'a> {
    pub name: NameAst,
    pub ty: TyAst<'a>,
}

impl<'a> Ast<'a> for FuncArgAst<'a> {
    type Args = ();

    const NAME: &'static str = "function argument";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        Some(Self {
            name: ctx.parse()?,
            ty: {
                ctx.expect_advance(TokenKind::Colon)?;
                ctx.parse()?
            },
        })
    }

    fn span(&self) -> Span {
        self.name.span().joined(self.ty.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FuncBodyAst<'a> {
    Arrow(Span, ExprAst<'a>),
    Block(BlockAst<'a>),
    Extern(Span),
}

impl<'a> Ast<'a> for FuncBodyAst<'a> {
    type Args = ();

    const NAME: &'static str = "function body";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            ThickRightArrow => {
                let arrow = ctx.advance().span;
                ctx.skip(TokenKind::NewLine);
                ctx.parse().map(|e| Self::Arrow(arrow, e))
            },
            LeftCurly => ctx.parse().map(Self::Block),
            Extern => Some(Self::Extern(ctx.advance().span)),
        }}
    }

    fn span(&self) -> Span {
        match self {
            Self::Arrow(span, expr) => span.joined(expr.span()),
            Self::Block(block) => block.span(),
            Self::Extern(span) => *span,
        }
    }
}

// impl Parser<'_> {
//     pub fn r#fn(&mut self) -> Option<()> {
//         let start = self.state.current.span;
//         let vis = self.signature_unfinished()?;

//         if self.at(TokenKind::Extern) {
//             self.capture(AstKind::None);
//         } else {
//             self.start();
//             let span = list!(self, LeftCurly, NewLine, RightCurly, expr)?;
//             self.finish(AstKind::FuncBody, span);
//         }

//         self.finish_last(AstKind::Func { vis }, start);

//         Ok(())
//     }

//     fn signature_unfinished(&mut self) -> Option<Vis> {
//         self.start();
//         self.advance();
//         let vis = self.visibility();
//         self.call_conv();
//         self.generics()?;
//         self.ident()?;
//         list!(self, LeftParen, Comma, RightParen, fn_arg)?;
//         if self.at(TokenKind::RightArrow) {
//             self.advance();
//             self.ty()?;
//         } else {
//             self.ast_data.cache(Ast::none());
//         }
//         Ok(vis)
//     }

//     pub fn signature(&mut self) -> Option<()> {
//         let start = self.state.current.span;
//         let vis = self.signature_unfinished()?;
//         self.finish_last(AstKind::FuncSignature { vis }, start);
//         Ok(())
//     }

//     fn call_conv(&mut self) {
//         if self.at(TokenKind::String) {
//             self.capture(AstKind::String);
//         } else {
//             self.ast_data.cache(Ast::none());
//         }
//     }

//     fn fn_arg(&mut self) -> Option<()> {
//         let start = self.start();
//         let mutable = self.advance_if(TokenKind::Mut);
//         self.ident()?;
//         self.expect(TokenKind::Colon)?;
//         self.advance();
//         self.ty()?;
//         self.finish_last(AstKind::FuncArg { mutable }, start);
//         Ok(())
//     }
// }
