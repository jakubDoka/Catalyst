use std::ops::Not;

use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn r#loop(&mut self) -> Option<LoopAst<'arena, M>> {
        Some(LoopAst {
            r#loop: self.advance(),
            label: self.at(Tk::Label).then(|| self.name_unchecked()),
            body: self.expr()?,
        })
    }

    pub fn r#break(&mut self) -> Option<BreakAst<'arena, M>> {
        Some(BreakAst {
            r#break: self.advance(),
            label: self.at(Tk::Label).then(|| self.name_unchecked()),
            value: self
                .at([Tk::NewLine, Tk::Else, Tk::Comma])
                .not()
                .then(|| self.expr())
                .transpose()?,
        })
    }

    pub fn r#continue(&mut self) -> Option<ContinueAst<M>> {
        Some(ContinueAst {
            r#continue: self.advance(),
            label: self.at(Tk::Label).then(|| self.name_unchecked()),
        })
    }

    pub fn r#if(&mut self) -> Option<IfAst<'arena, M>> {
        Some(IfAst {
            r#if: self.advance(),
            cond: self.expr()?,
            body: self.branch()?,
            elifs: {
                let mut else_ifs = bumpvec![];
                while self.try_advance_ignore_lines(Tk::Elif).is_some() {
                    else_ifs.push(self.elif()?);
                }
                self.arena.alloc_iter(else_ifs)
            },
            r#else: self
                .try_advance_ignore_lines(Tk::Else)
                .map(|r#else| self.branch().map(|body| (r#else, body)))
                .transpose()?,
        })
    }

    fn elif(&mut self) -> Option<ElifAst<'arena, M>> {
        Some(ElifAst {
            elif: self.advance(),
            cond: self.expr()?,
            body: self.branch()?,
        })
    }

    fn branch(&mut self) -> Option<BranchAst<'arena, M>> {
        branch!(self => {
            LeftBrace => self.object("branch block", Self::expr).map(BranchAst::Block),
            ThickRightArrow => Some(BranchAst::Arrow(self.advance(), {
                self.skip(Tk::NewLine);
                self.expr()?
            })),
            @"code branch",
        })
    }

    pub fn r#match(&mut self) -> Option<MatchExprAst<'arena, M>> {
        Some(MatchExprAst {
            r#match: self.advance(),
            expr: self.expr()?,
            body: self.object("match statement body", Self::match_arm)?,
        })
    }

    fn match_arm(&mut self) -> Option<MatchArmAst<'arena, M>> {
        Some(MatchArmAst {
            pattern: self.pat(None)?,
            body: self.branch()?,
        })
    }

    pub fn r#return(&mut self) -> Option<ReturnExprAst<'arena, M>> {
        Some(ReturnExprAst {
            r#return: self.advance(),
            expr: self
                .at([Tk::NewLine, Tk::Else, Tk::Comma])
                .not()
                .then(|| self.expr())
                .transpose()?,
        })
    }
}
