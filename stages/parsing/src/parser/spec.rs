use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn param(&mut self) -> Option<ParamAst<'arena, M>> {
        Some(ParamAst {
            name: self.name("generic parameter")?,
            specs: self.param_specs()?,
        })
    }

    pub fn param_specs(&mut self) -> Option<Option<ParamSpecsAst<'arena, M>>> {
        let Some(colon) = self.try_advance(Tk::Colon) else {
            return Some(None);
        };

        let first = self.spec_expr()?;
        let mut rest = bumpvec![];
        while let Some(mut plus) = self.try_advance("+") {
            self.append_newlines(&mut plus.meta);
            rest.push((plus, self.spec_expr()?));
        }

        Some(Some(ParamSpecsAst {
            colon,
            first,
            rest: self.arena.alloc_iter(rest),
        }))
    }

    pub fn spec_expr(&mut self) -> Option<SpecExprAst<'arena, M>> {
        Some(SpecExprAst {
            path: self.path(None)?,
        })
    }
}
