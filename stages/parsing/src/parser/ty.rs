use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn ty(&mut self) -> Option<TyAst<'arena, M>> {
        branch! {self => {
            Ident => {
                if self.at("_") {
                    return Some(TyAst::Wildcard(self.advance()));
                }

                self.path(None).map(TyAst::Path)
            },
            LeftParen => self.list("tuple", Self::ty, Tk::LeftParen, Tk::Comma, Tk::RightParen)
                .map(TyAst::Tuple),
            Operator(_ = 0) => branch!(str self => {
                "^" => self.pointer()
                    .map(|p| self.arena.alloc(p))
                    .map(TyAst::Pointer),
                @"prefixed type",
            }),
            @"type",
        }}
    }

    fn pointer(&mut self) -> Option<TyPointerAst<'arena, M>> {
        Some(TyPointerAst {
            carrot: self.advance(),
            mutability: self.mutability()?,
            ty: self.ty()?,
        })
    }

    pub fn mutability(&mut self) -> Option<Option<MutabilityAst<'arena, M>>> {
        Some(Some(branch! {self => {
            Mut => MutabilityAst::Mut(self.advance()),
            Use => MutabilityAst::Generic(self.advance(), self.path(None)?),
            @ => return Some(None),
        }}))
    }

    pub fn path(&mut self, leading_slash: Option<SourceInfo<M>>) -> Option<PathAst<'arena, M>> {
        Some(PathAst {
            slash: leading_slash,
            start: self.path_segment()?,
            segments: {
                let mut segments = bumpvec![];
                while self.at(Tk::BackSlash) {
                    self.advance();
                    segments.push(self.path_segment()?);
                }
                self.arena.alloc_iter(segments)
            },
        })
    }

    fn path_segment(&mut self) -> Option<PathSegmentAst<'arena, M>> {
        branch! {self => {
            Ident => Some(PathSegmentAst::Name(self.name_unchecked())),
            LeftBracket => self.array("path segment generics", Self::ty)
                .map(PathSegmentAst::Params),
            @"path segment",
        }}
    }
}
