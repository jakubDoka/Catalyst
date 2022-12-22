use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn pat(&mut self, mutable: Option<SourceInfo<M>>) -> Option<PatAst<'arena, M>> {
        branch!(self => {
            Mut => {
                if let Some(mutable) = mutable {
                    self.workspace.push(UselessMut {
                        prev: mutable.span,
                        loc: self.loc(),
                    });
                }

                let mutable = self.advance();
                self.pat(Some(mutable))
            },
            Ident => Some(branch! { str self => {
                "_" => PatAst::Wildcard(self.advance()),
                @ => PatAst::Binding(mutable, self.name_unchecked()),
            }}),
            BackSlash => {
                if self.next_at(TokenKind::Ident) {
                    self.enum_ctor_pat(mutable).map(|e| self.arena.alloc(e)).map(PatAst::EnumCtor)
                } else {
                    self.struct_ctor_pat(mutable).map(PatAst::StructCtor)
                }
            },
            Int => Some(PatAst::Int(self.advance())),
            @"pattern",
        })
    }

    fn enum_ctor_pat(
        &mut self,
        mutable: Option<SourceInfo<M>>,
    ) -> Option<EnumCtorPatAst<'arena, M>> {
        Some(EnumCtorPatAst {
            slash: self.advance(),
            name: self.name_unchecked(),
            value: self
                .try_advance(TokenKind::Tilde)
                .map(|tilde| self.pat(mutable).map(|value| (tilde, value)))
                .transpose()?,
        })
    }

    fn struct_ctor_pat(
        &mut self,
        mutable: Option<SourceInfo<M>>,
    ) -> Option<StructCtorPatAst<'arena, M>> {
        Some(StructCtorPatAst {
            slash: self.advance(),
            fields: self.object("struct constructor pattern", |s| {
                s.struct_ctor_pat_field(mutable)
            })?,
        })
    }

    fn struct_ctor_pat_field(
        &mut self,
        mutable: Option<SourceInfo<M>>,
    ) -> Option<StructCtorPatFieldAst<'arena, M>> {
        Some(branch! {self => {
            Mut => {
                if let Some(mutable) = mutable {
                    self.workspace.push(UselessMut {
                        prev: mutable.span,
                        loc: self.loc(),
                    });
                }

                let mutable = self.advance();
                self.struct_ctor_pat_field(Some(mutable))?
            },
            Ident => {
                let name = self.name_unchecked();
                branch! {self => {
                    Colon => StructCtorPatFieldAst::Named { name, colon: self.advance(), pat: self.pat(mutable)? },
                    @ => StructCtorPatFieldAst::Simple { name, mutable },
                }}
            },
            DoubleDot => StructCtorPatFieldAst::DoubleDot(self.advance()),
            @"struct field pattern",
        }})
    }
}

ctl_errors! {
    #[warn => "useless 'mut' in pattern"]
    #[info => "'mut' is recursive"]
    error UselessMut {
        #[info loc.origin, prev, "this already makes the pattern mutable"]
        #[warn loc]
        prev: Span,
        loc: SourceLoc,
    }
}
