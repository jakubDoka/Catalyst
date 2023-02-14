use middleware::*;

use crate::Fmt;

pub trait Length {
    fn length(&self, fmt: &mut Fmt) -> usize;
    fn length_low(&self, fmt: &mut Fmt) -> usize;
}

impl<T: Spanned> Length for T {
    default fn length(&self, fmt: &mut Fmt) -> usize {
        let span = self.span();
        if let Some(&cached) = fmt.fmt_ctx.compressed_lengths.get(&span) {
            return cached;
        }
        let length = self.length_low(fmt);
        fmt.fmt_ctx.compressed_lengths.insert(span, length);
        length
    }

    default fn length_low(&self, fmt: &mut Fmt) -> usize {
        fmt.cfg.max_line_width + 1
    }
}

impl Length for Span {
    fn length_low(&self, _fmt: &mut Fmt) -> usize {
        self.len()
    }
}

impl Length for NameAst<u32> {
    fn length_low(&self, _fmt: &mut Fmt) -> usize {
        self.span.len()
    }
}

impl Length for SourceInfo<u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.span.len() + fmt.source[self.after().range()].length_low(fmt)
    }
}

impl Length for str {
    fn length(&self, fmt: &mut Fmt) -> usize {
        self.length_low(fmt)
    }

    fn length_low(&self, fmt: &mut Fmt) -> usize {
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
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.name.length(fmt) + self.specs.length(fmt)
    }
}

impl Length for ParamSpecsAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.colon.length(fmt)
            + " ".len()
            + self.first.length(fmt)
            + self.rest.iter().fold(0, |acc, (plus, spec)| {
                acc + " ".len() + plus.length(fmt) + " ".len() + spec.length(fmt)
            })
    }
}

impl Length for SpecExprAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.path.length(fmt)
    }
}

impl Length for PathAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.slash.length(fmt)
            + self.start.length(fmt)
            + self
                .segments
                .iter()
                .fold(0, |acc, segment| acc + "/".len() + segment.length(fmt))
    }
}

impl Length for PathSegmentAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        match self {
            PathSegmentAst::Name(name) => name.length(fmt),
            PathSegmentAst::Params(params) => params.length(fmt),
        }
    }
}

impl Length for TyAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        match self {
            TyAst::Path(p) => p.length(fmt),
            TyAst::Pointer(p) => p.length(fmt),
            TyAst::Tuple(t) => t.length(fmt),
            TyAst::Array(a) => a.length(fmt),
            TyAst::Wildcard(w) => w.length(fmt),
        }
    }
}

impl Length for TyArrayAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.start.length(fmt)
            + self.ty.length(fmt)
            + self.semi.length(fmt)
            + " ".len()
            + self.size.length(fmt)
            + self.end.length(fmt)
    }
}

impl Length for TyPointerAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.carrot.length(fmt) + self.mutability.length(fmt) + self.ty.length(fmt)
    }
}

impl Length for MutabilityAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        (match self {
            MutabilityAst::Mut(m) => m.length(fmt),
            MutabilityAst::Generic(u, p) => u.length(fmt) + p.length(fmt),
        }) + " ".len()
    }
}

impl<T: Length> Length for Option<T> {
    fn length(&self, fmt: &mut Fmt) -> usize {
        self.length_low(fmt)
    }

    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.as_ref().map(|t| t.length(fmt)).unwrap_or(0)
    }
}

impl<T: Length> Length for ListAst<'_, T, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.start.length(fmt)
            + self.elements.iter().fold(0, |acc, item| {
                acc + item.value.length(fmt) + item.delim.length(fmt) + " ".len()
            })
            + self.end.length(fmt)
    }
}

impl Length for FuncArgAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.pat.length(fmt) + self.colon.length(fmt) + " ".len() + self.ty.length(fmt)
    }
}

impl Length for PatAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
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
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.slash.length(fmt) + self.fields.length(fmt)
    }
}

impl Length for StructCtorPatFieldAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
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
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.slash.length(fmt)
            + self.name.length(fmt)
            + match self.value {
                Some((tilde, value)) => tilde.length(fmt) + value.length(fmt),
                None => 0,
            }
    }
}

impl Length for ExprAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        match *self {
            ExprAst::Unit(unary) => unary.length(fmt),
            ExprAst::Binary(binary) => binary.length(fmt),
        }
    }
}

impl Length for UnitExprAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        use UnitExprAst::*;
        match *self {
            StructCtor(s) => s.length(fmt),
            EnumCtor(e) => e.length(fmt),
            DotExpr(d) => d.length(fmt),
            Call(c) => c.length(fmt),
            Path(p) => p.length(fmt),
            Return(r) => r.length(fmt),
            Int(i) | Float(i) | Char(i) | Bool(i) => i.length(fmt),
            Match(m) => m.length(fmt),
            If(i) => i.length(fmt),
            Loop(l) => l.length(fmt),
            Break(b) => b.length(fmt),
            Continue(c) => c.length(fmt),
            Let(l) => l.length(fmt),
            Deref(d, e) => d.length(fmt) + e.length(fmt),
            Ref(m, d, e) => m.length(fmt) + d.length(fmt) + d.is_some() as usize + e.length(fmt),
            Block(b) => b.length(fmt),
        }
    }
}

impl Length for LetAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt)
            + " ".len()
            + self.pat.length(fmt)
            + match self.ty {
                Some((colon, ty)) => colon.length(fmt) + " ".len() + ty.length(fmt),
                None => 0,
            }
            + self.equal.length(fmt)
            + " ".len()
            + self.value.length(fmt)
    }
}

impl Length for ContinueAst<u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt)
            + match self.label {
                Some(label) => " ".len() + label.length(fmt),
                None => 0,
            }
    }
}

impl Length for LoopAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt) + " ".len() + self.body.length(fmt)
    }
}

impl Length for BreakAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt)
            + match self.value {
                Some(value) => " ".len() + value.length(fmt),
                None => 0,
            }
    }
}

impl Length for MatchExprAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        fmt.cfg.max_line_width + 1
    }
}

impl Length for IfAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt)
            + " ".len()
            + self.cond.length(fmt)
            + " ".len()
            + self.body.length(fmt)
            + self
                .elifs
                .iter()
                .fold(0, |acc, elif| acc + " ".len() + elif.length(fmt))
            + match self.r#else {
                Some((keyword, r#else)) => {
                    " ".len() + keyword.length(fmt) + " ".len() + r#else.length(fmt)
                }
                None => 0,
            }
    }
}

impl Length for ElifAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt)
            + " ".len()
            + self.cond.length(fmt)
            + " ".len()
            + self.body.length(fmt)
    }
}

impl Length for BranchAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        match *self {
            BranchAst::Block(..) => fmt.cfg.max_line_width + 1,
            BranchAst::Arrow(arrow, expr) => arrow.length(fmt) + " ".len() + expr.length(fmt),
        }
    }
}

impl Length for BinaryExprAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.lhs.length(fmt) + " ".len() + self.op.length(fmt) + " ".len() + self.rhs.length(fmt)
    }
}

impl Length for StructCtorAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.path.length(fmt) + self.slash.length(fmt) + self.body.length(fmt)
    }
}

impl Length for StructCtorFieldAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.name.length(fmt)
            + match self.value {
                Some((colon, value)) => colon.length(fmt) + " ".len() + value.length(fmt),
                None => 0,
            }
    }
}

impl Length for EnumCtorAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.path.length(fmt)
            + match self.value {
                Some((tilde, value)) => tilde.length(fmt) + value.length(fmt),
                None => 0,
            }
    }
}

impl Length for DotExprAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.lhs.length(fmt) + self.infix.length(fmt) + self.rhs.length(fmt)
    }
}

impl Length for CallAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.callable.length(fmt) + self.args.length(fmt)
    }
}

impl Length for ReturnAst<'_, u32> {
    fn length_low(&self, fmt: &mut Fmt) -> usize {
        self.keyword.length(fmt) + " ".len() + self.expr.length(fmt)
    }
}
