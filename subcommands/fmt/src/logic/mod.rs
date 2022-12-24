use std::{
    iter, mem,
    ops::{Deref, DerefMut},
};

use crate::*;

pub struct Fmt<'ctx> {
    pub(crate) fmt_ctx: &'ctx mut FmtCtx,
    pub(crate) buffer: &'ctx mut String,
    pub(crate) cfg: &'ctx FmtCfg,
    pub(crate) source: &'ctx str,
}

impl<'ctx> Fmt<'ctx> {
    pub fn source(&mut self, (items, after, last): ItemsAstResult<u32>) {
        for (between, item) in items {
            if let Some(between) = between {
                self.white_space(between.full());
            }
            self.item(item);
        }

        if let Some(after) = after {
            self.white_space(after.full());
        }

        if let Some(last) = last {
            self.span(last.span);
        }
    }

    fn item(&mut self, item: ItemAst<u32>) {
        match item {
            ItemAst::Struct(r#struct) => self.r#struct(r#struct),
            ItemAst::Func(_) => todo!(),
            ItemAst::Spec(spec) => self.spec(spec),
            ItemAst::Impl(_) => todo!(),
            ItemAst::Enum(r#enum) => self.r#enum(r#enum),
            ItemAst::Attribute(attr) => {
                self.top_level_attr(attr);
                self.newline();
                return;
            }
        }

        self.newline();
        self.newline();
    }

    fn spec(&mut self, spec: &SpecAst<u32>) {
        self.vis(spec.vis);
        self.keyword(spec.keyword);
        self.generics(spec.generics);
        self.name(spec.name);
        self.body(Self::spec_item, spec.body);
    }

    fn spec_item(&mut self, item: &FuncSigAst<u32>) {
        self.func_sig(item);
    }

    fn func_sig(&mut self, sig: &FuncSigAst<u32>) {
        self.keyword(sig.keyword);
        self.generics(sig.generics);
        self.name(sig.name);
        self.opt_list(Self::func_sig_arg, sig.args);
        if let Some((arrow, ty)) = sig.ret {
            self.source_info(arrow);
            self.buffer.push(' ');
            self.ty(&ty);
        }
    }

    fn func_sig_arg(&mut self, arg: &FuncArgAst<u32>) {
        self.pat(&arg.pat);
        self.source_info(arg.colon);
        self.buffer.push(' ');
        self.ty(&arg.ty);
    }

    fn pat(&mut self, pat: &PatAst<u32>) {}

    fn r#enum(&mut self, r#enum: &EnumAst<u32>) {
        self.vis(r#enum.vis);
        self.keyword(r#enum.keyword);
        self.generics(r#enum.generics);
        self.name(r#enum.name);
        self.body(Self::enum_variant, r#enum.body);
    }

    fn enum_variant(&mut self, variant: &EnumVariantAst<u32>) {
        self.source_info(variant.name.source_info);
        let Some((colon, ty)) = variant.ty else {return};
        self.source_info(colon);
        self.buffer.push(' ');
        self.ty(&ty);
    }

    fn r#struct(&mut self, r#struct: &StructAst<u32>) {
        self.vis(r#struct.vis);
        self.keyword(r#struct.keyword);
        self.generics(r#struct.generics);
        self.name(r#struct.name);
        self.body(Self::struct_field, r#struct.body);
    }

    fn struct_field(&mut self, field: &StructFieldAst<u32>) {
        self.vis(field.vis);
        self.opt_source_info(field.used);
        self.opt_source_info(field.mutable);
        self.name(field.name);
        self.source_info(field.colon);
        self.ty(&field.ty);
    }

    fn ty(&mut self, ty: &TyAst<u32>) {
        match *ty {
            TyAst::Path(path) => self.path(path),
            TyAst::Tuple(tuple) => self.list(Self::ty, tuple),
            TyAst::Pointer(p) => {
                self.source_info(p.carrot);
                self.mutability(p.mutability);
                self.ty(&p.ty);
            }
            TyAst::Wildcard(w) => self.source_info(w),
        }
    }

    fn mutability(&mut self, mutability: Option<MutabilityAst<u32>>) {
        let Some(mutability) = mutability else {
            return;
        };

        match mutability {
            MutabilityAst::Mut(m) => self.keyword(m),
            MutabilityAst::Generic(u, t) => {
                self.source_info(u);
                self.path(t);
            }
        }
    }

    fn path(&mut self, path: PathAst<u32>) {
        self.opt_source_info(path.slash);
        self.path_segment(&path.start);
        for segment in path.segments {
            self.buffer.push('\\');
            self.path_segment(segment);
        }
    }

    fn path_segment(&mut self, segment: &PathSegmentAst<u32>) {
        match *segment {
            PathSegmentAst::Name(name) => self.source_info(name.source_info),
            PathSegmentAst::Params(params) => self.list(Self::ty, params),
        }
    }

    fn body<T>(&mut self, mut fmt: impl FnMut(&mut Self, &T), list: Option<ListAst<T, u32>>) {
        let Some(list) = list else {
            return;
        };

        self.buffer.push(' ');
        self.source_info(list.start);
        self.newline();
        self.dive();
        for &ListElemAst { ref value, delim } in list.elements {
            self.ident();
            fmt(self, value);
            if let Some(delim) = delim {
                self.white_space(delim.after());
            }
            self.newline();
        }
        self.emerge();
        self.ident();
        self.source_info(list.end);
    }

    fn generics(&mut self, generics: Option<ListAst<ParamAst<u32>, u32>>) {
        if let Some(generics) = generics {
            self.buffer.push(' ');
            self.list(Self::param, generics);
        }
    }

    fn param(&mut self, param: &ParamAst<u32>) {
        self.name(param.name);
        if let Some(specs) = param.specs {
            self.params_specs(specs);
        }
    }

    fn params_specs(&mut self, specs: ParamSpecsAst<u32>) {
        self.source_info(specs.colon);
        self.buffer.push(' ');
        self.path(specs.first.path);

        let mut indented = false;
        for &(plus, spec) in specs.rest {
            let len = " ".len() + plus.length(self) + " ".len() + spec.path.length(self);
            self.cond_dive(len, &mut indented);

            self.source_info(plus);
            self.buffer.push(' ');
            self.path(spec.path);
        }
        self.cond_emerge(indented);
    }

    fn opt_list<T: Length>(
        &mut self,
        fmt: impl FnMut(&mut Self, &T),
        list: Option<ListAst<T, u32>>,
    ) {
        if let Some(list) = list {
            self.list(fmt, list);
        }
    }

    fn list<T: Length>(&mut self, mut fmt: impl FnMut(&mut Self, &T), list: ListAst<T, u32>) {
        self.source_info(list.start);
        let Some((&ListElemAst { ref value, delim }, rest)) = list.elements.split_first() else {
            self.source_info(list.end);
            return;
        };

        fmt(self, value);
        self.opt_source_info(delim);

        let mut indented = false;
        for (i, &ListElemAst { ref value, delim }) in rest.iter().enumerate() {
            self.cond_dive(
                value.length(self) + delim.length(self) + " ".len(),
                &mut indented,
            );

            fmt(self, value);
            if i == rest.len() - 1 {
                if let Some(delim) = delim {
                    self.white_space(delim.after());
                }
            } else {
                self.opt_source_info(delim);
            }
        }
        self.cond_emerge(indented);

        self.source_info(list.end);
    }

    fn cond_dive(&mut self, len: usize, cond: &mut bool) {
        if self.fits(len) {
            self.buffer.push(' ');
        } else {
            if mem::replace(cond, true) {
                self.dive();
            }

            self.newline();
            self.ident();
        }
    }

    fn cond_emerge(&mut self, cond: bool) {
        if cond {
            self.emerge();
        }
    }

    fn top_level_attr(&mut self, attr: &TopLevelAttrAst<u32>) {
        self.source_info(attr.hash);
    }

    fn name(&mut self, name: NameAst<u32>) {
        if !self.at_newline() {
            self.buffer.push(' ');
        }

        self.source_info(name.source_info);
    }

    fn vis(&mut self, vis: Option<VisAst<u32>>) {
        if let Some(vis) = vis {
            self.source_info(vis.source_meta);
        }
    }

    fn opt_source_info(&mut self, info: Option<SourceInfo<u32>>) {
        if let Some(info) = info {
            self.source_info(info);
        }
    }

    fn keyword(&mut self, keyword: SourceInfo<u32>) {
        if !self.at_newline() {
            self.buffer.push(' ');
        }

        self.source_info(keyword);
    }

    fn source_info(&mut self, info: SourceInfo<u32>) {
        self.span(info.span);
        self.white_space(info.after());
    }

    fn white_space(&mut self, span: Span) {
        let mut new_lines = self.buffer.chars().rev().take_while(|c| *c == '\n').count();
        for (tok, span) in SkippedToken::lexer(&self.source[span.range()]).spanned() {
            match tok {
                SkippedToken::Comment | SkippedToken::MultiComment => {
                    if self.at_newline() {
                        self.ident();
                    } else {
                        self.buffer.push(' ');
                    }
                    self.span(Span::new(span));
                }
                SkippedToken::NewLine => {
                    if new_lines < self.cfg.max_newlines {
                        self.newline();
                        new_lines += 1;
                    }
                    continue;
                }
                SkippedToken::Space | SkippedToken::Error => (),
            }

            new_lines = 0;
        }
    }

    fn fits(&self, len: usize) -> bool {
        self.buffer.len() - self.last_newline + len <= self.cfg.max_line_width
    }

    fn span(&mut self, span: Span) {
        self.handle_span_newlines(span);
        self.buffer.push_str(&self.source[span.range()]);
    }

    fn ident(&mut self) {
        self.buffer.extend(iter::repeat(' ').take(self.indent * 4));
    }

    fn newline(&mut self) {
        self.buffer.push('\n');
        self.last_newline = self.buffer.len();
    }

    fn at_newline(&self) -> bool {
        self.last_newline == self.buffer.len()
    }

    fn handle_span_newlines(&mut self, span: Span) {
        let Some(index) = self.source[span.range()].rfind('\n') else {
            return;
        };

        self.last_newline = span.start as usize + index + 1;
    }
}

impl Deref for Fmt<'_> {
    type Target = FmtCtx;

    fn deref(&self) -> &Self::Target {
        self.fmt_ctx
    }
}

impl DerefMut for Fmt<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.fmt_ctx
    }
}

#[derive(Default)]
pub struct FmtCtx {
    last_newline: usize,
    indent: usize,
    folding: bool,

    compressed_lengths: Map<Span, usize>,
}

impl FmtCtx {
    fn fold(&mut self) -> Folding {
        Folding(mem::replace(&mut self.folding, true))
    }

    fn unfold(&mut self, folding: Folding) {
        self.folding = folding.0;
    }

    fn dive(&mut self) {
        self.indent += 1;
    }

    fn emerge(&mut self) {
        self.indent -= 1;
    }
}

#[must_use]
struct Folding(bool);

pub struct FmtCfg {
    pub max_line_width: usize,
    pub max_newlines: usize,
    /// if the average length of list item is greater, we distribute
    /// item per line, otherwise we try to fit as many items as possible
    pub max_list_item_length: usize,
}

impl FmtCfg {
    pub const DEFAULT: Self = Self {
        max_line_width: 80,
        max_newlines: 2,
        max_list_item_length: 20,
    };
}
