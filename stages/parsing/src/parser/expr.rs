use super::*;

list_meta!(BlockMeta LeftCurly NewLine RightCurly);
pub type BlockAst<'a> = ListAst<'a, ExprAst<'a>, BlockMeta>;

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'a> {
    Unit(&'a UnitExprAst<'a>),
    Binary(&'a BinaryExprAst<'a>),
}

impl<'a> Ast<'a> for ExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let unit = ctx.parse_alloc().map(ExprAst::Unit);
        if let TokenKind::Operator(precedence) = ctx.state.current.kind {
            let op = ctx.name_unchecked();
            ctx.parse_args_alloc((unit?, op, precedence))
                .map(Self::Binary)
        } else {
            unit
        }
    }

    fn span(&self) -> Span {
        match *self {
            ExprAst::Unit(unit) => unit.span(),
            ExprAst::Binary(binary) => binary.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExprAst<'a> {
    pub lhs: ExprAst<'a>,
    pub op: NameAst,
    pub rhs: ExprAst<'a>,
}

impl<'a> BinaryExprAst<'a> {
    pub fn new(lhs: ExprAst<'a>, op: NameAst, rhs: ExprAst<'a>) -> Self {
        Self { lhs, op, rhs }
    }
}

impl<'a> Ast<'a> for BinaryExprAst<'a> {
    type Args = (ExprAst<'a>, NameAst, u8);

    const NAME: &'static str = "binary expr";

    fn parse_args_internal(
        ctx: &mut ParsingCtx<'_, 'a>,
        (mut lhs, mut op, mut precedence): Self::Args,
    ) -> Result<Self, ()> {
        Ok(loop {
            let rhs = ctx.parse_alloc().map(ExprAst::Unit)?;

            let TokenKind::Operator(next_precedence) = ctx.state.current.kind else {
                break Self::new(lhs, op, rhs);
            };

            if precedence < next_precedence {
                lhs = ExprAst::Binary(ctx.arena.alloc(Self::new(lhs, op, rhs)));
                op = ctx.name_unchecked();
                precedence = next_precedence;
            } else {
                let op = ctx.name_unchecked();
                let rhs = ctx
                    .parse_args_alloc((rhs, op, next_precedence))
                    .map(ExprAst::Binary)?;
                break Self::new(lhs, op, rhs);
            }
        })
    }

    fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnitExprAst<'a> {
    Path(PathAst<'a>),
}

impl<'a> Ast<'a> for UnitExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "unit expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        branch!(ctx => {
            Ident => PathAst::parse(ctx).map(Self::Path),
            BackSlash => PathAst::parse(ctx).map(Self::Path),
        })
    }

    fn span(&self) -> Span {
        match *self {
            UnitExprAst::Path(path) => path.span(),
        }
    }
}

// impl Parser<'_> {
//     pub fn expr(&mut self) -> errors::Result {
//         let start = self.start();
//         self.unit_expr()?;
//         if let TokenKind::Operator(precedence) = self.state.current.kind {
//             self.op()?;
//             self.binary_expr(precedence, start)?;
//         } else {
//             self.join_frames();
//         }

//         Ok(())
//     }

//     pub fn binary_expr(&mut self, precedence: u8, start: Span) -> errors::Result {
//         loop {
//             self.expr()?;

//             let TokenKind::Operator(next_precedence) = self.state.current.kind else {
//                 self.finish_last(AstKind::Binary, start);
//                 break;
//             };

//             if precedence < next_precedence {
//                 self.finish_last(AstKind::Binary, start);
//                 self.start_with(1);
//                 self.op()?;
//             } else {
//                 self.op()?;
//                 self.binary_expr(next_precedence, start)?;
//                 self.finish_last(AstKind::Binary, start);
//                 break;
//             }
//         }

//         Ok(())
//     }

//     fn unit_expr(&mut self) -> errors::Result {
//         branch! { self => {
//             Return => self.r#return()?,
//             Int => self.capture(AstKind::Int),
//             Ident => self.ident_expr(true)?,
//         }};

//         Ok(())
//     }

//     fn ident_expr(&mut self, has_tail: bool) -> errors::Result {
//         self.ident_chain()?;

//         if self.at(TokenKind::Tick) && self.next(TokenKind::LeftBracket) {
//             self.instance_expr()?;
//         }

//         if has_tail {
//             if self.at(TokenKind::Tick) && self.next(TokenKind::LeftCurly) {
//                 self.struct_expr()?;
//                 return Ok(());
//             }

//             loop {
//                 branch! {self => {
//                     LeftBracket => self.index_expr()?,
//                     LeftParen => self.call_expr()?,
//                     Dot => self.dot_expr()?,
//                     _ => {
//                         if self.reduce_repetition(TokenKind::NewLine) && self.next(TokenKind::Dot) {
//                             self.advance();
//                             self.dot_expr()?;
//                         } else {
//                             break;
//                         }
//                     },
//                 }};
//             }
//         }

//         Ok(())
//     }

//     fn dot_expr(&mut self) -> errors::Result {
//         let start = self.start_with(1);
//         self.advance();
//         self.ident_expr(false)?;
//         self.finish_last(AstKind::DotExpr, start);

//         Ok(())
//     }

//     fn call_expr(&mut self) -> errors::Result {
//         let start = self.start_with(1);
//         let end = list!(self, LeftParen, Comma, RightParen, expr)?;
//         self.finish(AstKind::Call, start.joined(end));

//         Ok(())
//     }

//     fn index_expr(&mut self) -> errors::Result {
//         let start = self.start_with(1);
//         self.advance();
//         self.expr()?;
//         self.expect(TokenKind::RightBracket)?;
//         let end = self.state.current.span;
//         self.advance();
//         self.finish(AstKind::Index, start.joined(end));
//         Ok(())
//     }

//     fn instance_expr(&mut self) -> errors::Result {
//         let start = self.start_with(1);
//         let end = list!(self, LeftBracket, Comma, RightBracket, ty)?;
//         self.finish(AstKind::InstanceExpr, start.joined(end));
//         Ok(())
//     }

//     fn struct_expr(&mut self) -> errors::Result {
//         let start = self.start_with(1);
//         let end = list!(self, LeftBracket, Comma, RightBracket, struct_expr_field)?;
//         self.finish(AstKind::StructExprBody, end);
//         self.finish(AstKind::StructExpr, start.joined(end));
//         Ok(())
//     }

//     fn struct_expr_field(&mut self) -> errors::Result {
//         let start = self.start();
//         self.ident()?;
//         self.expect(TokenKind::Colon)?;
//         self.advance();
//         self.expr()?;
//         self.finish_last(AstKind::StructExprField, start);
//         Ok(())
//     }

//     fn r#return(&mut self) -> errors::Result {
//         let start = self.start();
//         self.advance();
//         if self.at(TokenKind::NewLine) {
//             self.ast_data.cache(Ast::none());
//             self.finish(AstKind::Return, start);
//         } else {
//             self.expr()?;
//             self.finish_last(AstKind::Return, start)
//         };
//         Ok(())
//     }

//     pub fn op(&mut self) -> errors::Result {
//         let start = self.start();
//         self.capture(AstKind::Operator);
//         if self.at(TokenKind::Tick) {
//             self.advance();
//             self.capture(AstKind::Ident);
//             self.finish_last(AstKind::OperatorWithModule, start);
//         } else {
//             self.join_frames();
//         }

//         Ok(())
//     }
// }
