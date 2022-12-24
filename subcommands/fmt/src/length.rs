use middleware::*;

use crate::Fmt;

pub trait Length {
    fn length(&self, fmt: &Fmt) -> usize;
}

impl Length for Span {
    fn length(&self, _fmt: &Fmt) -> usize {
        self.len()
    }
}

impl Length for NameAst<u32> {
    fn length(&self, _fmt: &Fmt) -> usize {
        self.span.len()
    }
}

impl Length for SourceInfo<u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.span.len() + fmt.source[self.after().range()].length(fmt)
    }
}

impl Length for str {
    fn length(&self, fmt: &Fmt) -> usize {
        let mut length = 0;
        for (tok, span) in SkippedToken::lexer(self).spanned() {
            match tok {
                SkippedToken::MultiComment => {
                    length += span.len() + 1;
                    if fmt.source[span].contains('\n') {
                        return fmt.cfg.max_line_width + 1;
                    }
                }
                SkippedToken::Comment | SkippedToken::NewLine => {
                    return fmt.cfg.max_line_width + 1;
                }
                SkippedToken::Space | SkippedToken::Error => (),
            }
        }
        length
    }
}

impl Length for ParamAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.name.length(fmt) + self.specs.length(fmt)
    }
}

impl Length for ParamSpecsAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.colon.length(fmt)
            + " ".len()
            + self.first.length(fmt)
            + self.rest.iter().fold(0, |acc, (plus, spec)| {
                acc + " ".len() + plus.length(fmt) + " ".len() + spec.length(fmt)
            })
    }
}

impl Length for SpecExprAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.path.length(fmt)
    }
}

impl Length for PathAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.slash.length(fmt)
            + self.start.length(fmt)
            + self
                .segments
                .iter()
                .fold(0, |acc, segment| acc + "/".len() + segment.length(fmt))
    }
}

impl Length for PathSegmentAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        match self {
            PathSegmentAst::Name(name) => name.length(fmt),
            PathSegmentAst::Params(params) => params.length(fmt),
        }
    }
}

impl Length for TyAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        match self {
            TyAst::Path(p) => p.length(fmt),
            TyAst::Pointer(p) => p.length(fmt),
            TyAst::Tuple(t) => t.length(fmt),
            TyAst::Wildcard(w) => w.length(fmt),
        }
    }
}

impl Length for TyPointerAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.carrot.length(fmt) + self.mutability.length(fmt) + self.ty.length(fmt)
    }
}

impl Length for MutabilityAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        (match self {
            MutabilityAst::Mut(m) => m.length(fmt),
            MutabilityAst::Generic(u, p) => u.length(fmt) + p.length(fmt),
        }) + " ".len()
    }
}

impl<T: Length> Length for Option<T> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.as_ref().map(|t| t.length(fmt)).unwrap_or(0)
    }
}

impl<T: Length> Length for ListAst<'_, T, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.start.length(fmt)
            + self.elements.iter().fold(0, |acc, item| {
                acc + item.value.length(fmt) + item.delim.length(fmt) + " ".len()
            })
            + self.end.length(fmt)
    }
}

impl Length for FuncArgAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.pat.length(fmt) + self.colon.length(fmt) + " ".len() + self.ty.length(fmt)
    }
}

impl Length for PatAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        match self {
            PatAst::Binding(m, p) => m.length(fmt) + p.length(fmt),
            PatAst::Wildcard(v) => v.length(fmt),
            PatAst::StructCtor(s) => s.length(fmt),
            PatAst::EnumCtor(e) => e.length(fmt),
            PatAst::Int(source_info) => source_info.length(fmt),
        }
    }
}

impl Length for StructCtorPatAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.slash.length(fmt) + self.fields.length(fmt)
    }
}

impl Length for StructCtorPatFieldAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        match self {
            StructCtorPatFieldAst::Simple { mutable, name } => {
                mutable.length(fmt) + mutable.is_some() as usize + name.length(fmt)
            }
            StructCtorPatFieldAst::Named { name, colon, pat } => {
                name.length(fmt) + colon.length(fmt) + " ".len() + pat.length(fmt)
            }
            StructCtorPatFieldAst::DoubleDot(source_info) => source_info.length(fmt),
        }
    }
}

impl Length for EnumCtorPatAst<'_, u32> {
    fn length(&self, fmt: &Fmt) -> usize {
        self.slash.length(fmt)
            + self.name.length(fmt)
            + match self.value {
                Some((tilde, value)) => tilde.length(fmt) + value.length(fmt),
                None => 0,
            }
    }
}
