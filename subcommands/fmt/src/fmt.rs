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
    pub fn manifest(&mut self, manifest: ManifestAst<u32>) {
        self.fmt_ctx.clear();
        if let Some(header) = manifest.header {
            self.white_space(header.full(), 0);
        }

        for (item, whitespace) in manifest.items {
            self.manifest_item(item);
            if let Some(whitespace) = whitespace {
                self.white_space(whitespace.full(), 1);
            }
        }
    }

    fn manifest_item(&mut self, item: &ManifestItemAst<u32>) {
        match *item {
            ManifestItemAst::Deps(deps) => self.manifest_deps(deps),
            ManifestItemAst::Field(ref field) => self.manifest_field(field),
        }
    }

    fn manifest_deps(&mut self, deps: ManifestDepsAst<u32>) {
        self.source_info(deps.deps);
        self.buffer.push(' ');
        self.block(Self::manifest_dep, deps.list);
    }

    fn manifest_field(&mut self, field: &ManifestFieldAst<u32>) {
        self.source_info(field.name.source_info);
        self.source_info(field.colon);
        self.buffer.push(' ');
        self.manifest_value(&field.value);
    }

    fn manifest_value(&mut self, value: &ManifestValueAst<u32>) {
        match *value {
            ManifestValueAst::Bool(literal) | ManifestValueAst::String(literal) => {
                self.source_info(literal)
            }
            ManifestValueAst::Array(array) => self.list(Self::manifest_value, array),
            ManifestValueAst::Object(object) => self.block(Self::manifest_field, object),
        }
    }

    fn manifest_dep(&mut self, dep: &ManifestDepAst<u32>) {
        if let Some(git) = dep.git {
            self.source_info(git);
            self.buffer.push(' ');
        }

        if let Some(name) = dep.name {
            self.source_info(name.source_info);
            self.buffer.push(' ');
        }

        self.source_info(dep.path);

        if let Some(version) = dep.version {
            self.buffer.push(' ');
            self.source_info(version);
        }
    }

    pub fn imports(&mut self, header: Option<SourceInfo<u32>>, imports: Option<ImportsAst<u32>>) {
        self.fmt_ctx.clear();
        if let Some(header) = header {
            self.white_space(header.full(), 0);
        }

        let Some(imports) = imports else {
            return;
        };

        self.source_info(imports.keyword);
        self.buffer.push(' ');
        self.block(Self::import, imports.items);
    }

    pub fn import(&mut self, import: &ImportAst<u32>) {
        if let Some(vis) = &import.vis {
            self.source_info(vis.source_meta);
            self.buffer.push(' ');
        }
        if let Some(name) = &import.name {
            self.source_info(name.source_info);
            self.buffer.push(' ');
        }
        self.source_info(import.path);
    }

    pub fn source(
        &mut self,
        ItemsAstResult {
            items,
            between,
            last,
        }: ItemsAstResult<u32>,
    ) {
        for (between, item) in items {
            if let Some(between) = between {
                let spacing = 1 + !matches!(item, ItemAst::Attribute(..)) as usize;
                self.white_space(between.full(), spacing);
            }
            self.item(item);
        }

        if let Some(after) = between {
            self.white_space(after.full(), 2);
        }

        if let Some(last) = last {
            self.source_info(last);
        }
    }

    fn item(&mut self, item: ItemAst<u32>) {
        match item {
            ItemAst::Struct(r#struct) => self.r#struct(r#struct),
            ItemAst::Func(func) => self.func(func),
            ItemAst::Spec(spec) => self.spec(spec),
            ItemAst::Impl(r#impl) => self.r#impl(r#impl),
            ItemAst::Enum(r#enum) => self.r#enum(r#enum),
            ItemAst::Attribute(attr) => {
                self.top_level_attr(attr);
                self.newline_if_none();
                return;
            }
            ItemAst::Const(consts) => self.r#const(consts),
        }

        self.newline_if_none();
        self.newline_if_none();
    }

    fn r#const(&mut self, r#const: &ConstAst<u32>) {
        self.vis(r#const.vis);
        self.keyword(r#const.keyword);

        self.name(r#const.name);

        self.source_info(r#const.colon);
        self.buffer.push(' ');
        self.ty(&r#const.ty);

        self.buffer.push(' ');
        self.source_info(r#const.eqal);
        self.buffer.push(' ');

        self.expr(&r#const.value);
    }

    fn r#impl(&mut self, r#impl: &ImplAst<u32>) {
        self.vis(r#impl.vis);
        self.keyword(r#impl.keyword);
        self.generics(r#impl.generics);
        self.impl_target(&r#impl.target);
        self.body(Self::impl_item, r#impl.body);
    }

    fn impl_target(&mut self, target: &ImplTargetAst<u32>) {
        match target {
            ImplTargetAst::Direct(ty) => self.ty(ty),
            ImplTargetAst::Spec(spec, keyword, ty) => {
                self.path(spec.path);
                self.buffer.push(' ');
                self.source_info(*keyword);
                self.buffer.push(' ');
                self.ty(ty);
            }
        }
    }

    fn impl_item(&mut self, item: &ImplItemAst<u32>) {
        match item {
            ImplItemAst::Func(func) => self.func(func),
        }
    }

    fn func(&mut self, func: &FuncDefAst<u32>) {
        self.vis(func.vis);
        self.func_sig(&func.signature);
        self.func_body(&func.body);
    }

    fn func_body(&mut self, block: &FuncBodyAst<u32>) {
        match *block {
            FuncBodyAst::Arrow(arrow, expr) => {
                self.buffer.push(' ');
                self.source_info(arrow);
                let len = expr.length(self);
                if !self.fits(len) {
                    self.dive();
                    self.newline_if_none();
                    self.indent();
                    self.expr(&expr);
                    self.emerge();
                } else {
                    self.buffer.push(' ');
                    self.expr(&expr);
                }
            }
            FuncBodyAst::Block(block) => {
                self.body(Self::expr, Some(block));
            }
            FuncBodyAst::Extern(source_info) => {
                self.buffer.push(' ');
                self.source_info(source_info);
            }
        }
    }

    fn expr(&mut self, expr: &ExprAst<u32>) {
        let mut state = ExprFmtState::default();
        self.expr_low(expr, &mut state);
        state.emerge(self);
    }

    fn expr_low(&mut self, expr: &ExprAst<u32>, state: &mut ExprFmtState) {
        match *expr {
            ExprAst::Unit(unit) => self.unit_expr(unit, state),
            ExprAst::Binary(binary) => self.binary_expr(binary, state),
        }
    }

    fn unit_expr(&mut self, unit: &UnitExprAst<u32>, state: &mut ExprFmtState) {
        match *unit {
            UnitExprAst::StructCtor(ref s) => self.struct_ctor(s),
            UnitExprAst::EnumCtor(e) => self.enum_ctor(e),
            UnitExprAst::DotExpr(d) => self.dot_expr(d, state),
            UnitExprAst::Call(c) => self.call(c, state),
            UnitExprAst::Path(p) => self.path(p),
            UnitExprAst::Return(r) => self.r#return(r),
            UnitExprAst::Int(source_info) => self.source_info(source_info),
            UnitExprAst::Char(source_info) => self.source_info(source_info),
            UnitExprAst::Bool(source_info) => self.source_info(source_info),
            UnitExprAst::Match(m) => self.r#match(m),
            UnitExprAst::If(i) => self.r#if(i),
            UnitExprAst::Loop(l) => self.r#loop(l),
            UnitExprAst::Break(b) => self.r#break(b),
            UnitExprAst::Continue(c) => self.r#continue(c),
            UnitExprAst::Let(l) => self.r#let(l),
            UnitExprAst::Deref(star, expr) => {
                self.source_info(star);
                self.unit_expr(expr, state);
            }
            UnitExprAst::Ref(carrot, mutability, expr) => {
                self.source_info(carrot);
                self.mutability(mutability);
                self.unit_expr(expr, state);
            }
            UnitExprAst::Block(block) => self.block(Self::expr, block),
        }
    }

    fn r#let(&mut self, r#let: LetAst<u32>) {
        self.source_info(r#let.keyword);
        self.buffer.push(' ');
        self.pat(&r#let.pat);

        if let Some((colon, ty)) = r#let.ty {
            self.source_info(colon);
            self.buffer.push(' ');
            self.ty(&ty);
        }

        self.buffer.push(' ');
        self.source_info(r#let.equal);
        self.buffer.push(' ');
        self.expr(&r#let.value);
    }

    fn r#continue(&mut self, r#continue: ContinueAst<u32>) {
        self.source_info(r#continue.keyword);
        if let Some(label) = r#continue.label {
            self.buffer.push(' ');
            self.source_info(label.source_info);
        }
    }

    fn r#break(&mut self, r#break: BreakAst<u32>) {
        self.source_info(r#break.keyword);
        if let Some(label) = r#break.label {
            self.buffer.push(' ');
            self.source_info(label.source_info);
        }
        if let Some(value) = r#break.value {
            self.buffer.push(' ');
            self.expr(&value);
        }
    }

    fn r#loop(&mut self, r#loop: LoopAst<u32>) {
        self.source_info(r#loop.keyword);
        self.buffer.push(' ');
        self.expr(&r#loop.body);
    }

    fn r#if(&mut self, r#if: IfAst<u32>) {
        let fold = !r#if.elifs.is_empty();

        self.source_info(r#if.keyword);
        self.buffer.push(' ');
        self.expr(&r#if.cond);
        self.buffer.push(' ');
        self.branch(r#if.body);

        let mut add_newline = matches!(r#if.body, BranchAst::Arrow(..));
        for elif in r#if.elifs {
            if add_newline {
                self.newline_if_none();
                self.indent();
            } else {
                self.buffer.push(' ');
            }
            add_newline = self.elif(elif);
        }

        if add_newline || fold {
            self.newline_if_none();
            self.indent();
        } else {
            self.buffer.push(' ');
        }

        if let Some((keyword, branch)) = r#if.r#else {
            self.source_info(keyword);
            self.buffer.push(' ');
            self.branch(branch);
        }
    }

    fn elif(&mut self, elif: &ElifAst<u32>) -> bool {
        self.source_info(elif.keyword);
        self.buffer.push(' ');
        self.expr(&elif.cond);
        self.buffer.push(' ');
        self.branch(elif.body);

        matches!(elif.body, BranchAst::Arrow(..))
    }

    fn r#match(&mut self, r#match: MatchExprAst<u32>) {
        self.source_info(r#match.keyword);
        self.buffer.push(' ');
        self.expr(&r#match.expr);
        self.buffer.push(' ');
        self.block(Self::match_arm, r#match.body);
    }

    fn match_arm(&mut self, arm: &MatchArmAst<u32>) {
        self.pat(&arm.pattern);
        self.buffer.push(' ');
        self.branch(arm.body);
    }

    fn branch(&mut self, branch: BranchAst<u32>) {
        match branch {
            BranchAst::Block(b) => self.block(Self::expr, b),
            BranchAst::Arrow(arrow, expr) => {
                self.source_info(arrow);
                let len = expr.length(self) + " ".len();
                let mut indented = false;
                self.cond_dive(len, &mut indented);
                self.expr(&expr);
                self.cond_emerge(&mut indented);
            }
        }
    }

    fn r#return(&mut self, r#return: ReturnAst<u32>) {
        self.source_info(r#return.keyword);
        if let Some(expr) = r#return.expr {
            self.buffer.push(' ');
            self.expr(&expr);
        }
    }

    fn call(&mut self, call: &CallAst<u32>, state: &mut ExprFmtState) {
        self.unit_expr(&call.callable, state);
        self.list(Self::expr, call.args);
    }

    fn dot_expr(&mut self, dot: &DotExprAst<u32>, state: &mut ExprFmtState) {
        self.unit_expr(&dot.lhs, state);
        let len = dot.rhs.length(self) + dot.infix.length(self);
        if !self.fits(len) {
            state.indent_dot(self);
        }
        self.buffer.push('.');
        self.path(dot.rhs);
    }

    fn enum_ctor(&mut self, enum_ctor: EnumCtorAst<u32>) {
        self.path(enum_ctor.path);
        if let Some((tilde, ref value)) = enum_ctor.value {
            self.source_info(tilde);
            self.expr(value);
        }
    }

    fn struct_ctor(&mut self, r#struct: &StructCtorAst<u32>) {
        if let Some(path) = r#struct.path {
            self.path(path);
        }
        self.source_info(r#struct.slash);
        self.list(Self::struct_ctor_field, r#struct.body);
    }

    fn struct_ctor_field(&mut self, field: &StructCtorFieldAst<u32>) {
        self.source_info(field.name.source_info);
        if let Some((colon, ref expr)) = field.value {
            self.source_info(colon);
            self.buffer.push(' ');
            self.expr(expr);
        }
    }

    fn binary_expr(&mut self, binary: &BinaryExprAst<u32>, state: &mut ExprFmtState) {
        let op_len = " ".len() + binary.op.length(self) + " ".len();
        self.expr_low(&binary.lhs, state);
        let rhs_len = binary.rhs.length(self);
        self.buffer.push(' ');
        self.source_info(binary.op.source_info);
        if self.fits(rhs_len + op_len) {
            self.buffer.push(' ');
        } else {
            state.indent_op(self);
        }
        self.expr_low(&binary.rhs, state);
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
            self.buffer.push(' ');
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

    fn pat(&mut self, pat: &PatAst<u32>) {
        match *pat {
            PatAst::Binding(m, b) => {
                self.opt_keyword(m);
                self.source_info(b.source_info);
            }
            PatAst::Wildcard(source_info) => self.source_info(source_info),
            PatAst::StructCtor(s) => self.struct_ctor_pat(s),
            PatAst::EnumCtor(e) => self.enum_ctor_pat(e),
            PatAst::Int(source_info) => self.source_info(source_info),
        }
    }

    fn enum_ctor_pat(&mut self, e: &EnumCtorPatAst<u32>) {
        self.source_info(e.slash);
        self.source_info(e.name.source_info);
        let Some((carrot, value)) = e.value else {return};
        self.source_info(carrot);
        self.pat(&value);
    }

    fn struct_ctor_pat(&mut self, s: StructCtorPatAst<u32>) {
        self.source_info(s.slash);
        self.list(Self::struct_ctor_pat_field, s.fields);
    }

    fn struct_ctor_pat_field(&mut self, field: &StructCtorPatFieldAst<u32>) {
        match *field {
            StructCtorPatFieldAst::Simple { mutable, name } => {
                self.opt_keyword(mutable);
                self.source_info(name.source_info);
            }
            StructCtorPatFieldAst::Named { name, colon, pat } => {
                self.source_info(name.source_info);
                self.source_info(colon);
                self.buffer.push(' ');
                self.pat(&pat);
            }
            StructCtorPatFieldAst::DoubleDot(source_info) => self.source_info(source_info),
        }
    }

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
        self.opt_keyword(field.used);
        self.opt_keyword(field.mutable);
        self.source_info(field.name.source_info);
        self.source_info(field.colon);
        self.buffer.push(' ');
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

    fn block<T>(&mut self, mut fmt: impl FnMut(&mut Self, &T), list: ListAst<T, u32>) {
        self.dive();
        self.source_info(list.start);
        self.newline_if_none();
        for &ListElemAst { ref value, delim } in list.elements {
            self.indent();
            fmt(self, value);
            self.newline_if_none();
            if let Some(delim) = delim {
                self.white_space(delim.after(), 0);
            }
        }
        self.emerge();
        self.indent();
        self.source_info(list.end);
    }

    fn body<T>(&mut self, fmt: impl FnMut(&mut Self, &T), body: Option<ListAst<T, u32>>) {
        let Some(list) = body else {
            return;
        };

        self.buffer.push(' ');
        self.block(fmt, list);
    }

    fn generics(&mut self, generics: Option<ListAst<ParamAst<u32>, u32>>) {
        if let Some(generics) = generics {
            self.list(Self::param, generics);
            self.buffer.push(' ');
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
        self.cond_emerge(&mut indented);
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
            let len = value.length(self) + delim.length(self) + " ".len();
            self.cond_dive(len, &mut indented);

            fmt(self, value);
            if i == rest.len() - 1 {
                if let Some(delim) = delim {
                    self.white_space(delim.after(), 0);
                }
            } else {
                self.opt_source_info(delim);
            }
        }
        self.cond_emerge(&mut indented);

        self.source_info(list.end);
    }

    fn cond_dive(&mut self, len: usize, cond: &mut bool) {
        if self.fits(len) {
            self.buffer.push(' ');
        } else {
            self.cond_dive_low(cond);
        }
    }

    fn cond_dive_low(&mut self, cond: &mut bool) {
        if !mem::replace(cond, true) {
            self.dive();
        }

        self.newline_if_none();
        self.indent();
    }

    fn cond_emerge(&mut self, cond: &mut bool) {
        if mem::take(cond) {
            self.emerge();
        }
    }

    fn top_level_attr(&mut self, attr: &TopLevelAttrAst<u32>) {
        self.source_info(attr.hash);
        self.wrapped(Self::top_level_attr_kind, attr.value);
    }

    fn top_level_attr_kind(&mut self, value: &TopLevelAttrKindAst<u32>) {
        match *value {
            TopLevelAttrKindAst::Entry(info)
            | TopLevelAttrKindAst::WaterDrop(info)
            | TopLevelAttrKindAst::NoMoves(info)
            | TopLevelAttrKindAst::CompileTime(info) => self.source_info(info),
            TopLevelAttrKindAst::Macro(keyword, name) => {
                self.source_info(keyword);
                self.buffer.push(' ');
                self.name(name);
            }
            TopLevelAttrKindAst::Inline(keyword, mode) => {
                self.source_info(keyword);
                if let Some(mode) = mode {
                    self.wrapped(Self::inline_mode, mode);
                }
            }
        }
    }

    fn inline_mode(&mut self, mode: &InlineModeAst<u32>) {
        match *mode {
            InlineModeAst::Always(info) | InlineModeAst::Never(info) => self.source_info(info),
        }
    }

    fn wrapped<T>(&mut self, fmt: impl FnOnce(&mut Self, &T), wrapped: WrappedAst<T, u32>) {
        self.source_info(wrapped.start);
        fmt(self, &wrapped.value);
        self.source_info(wrapped.end);
    }

    fn name(&mut self, name: NameAst<u32>) {
        self.source_info(name.source_info);
    }

    fn vis(&mut self, vis: Option<VisAst<u32>>) {
        if let Some(vis) = vis {
            self.source_info(vis.source_meta);
            self.buffer.push(' ');
        }
    }

    fn opt_source_info(&mut self, info: Option<SourceInfo<u32>>) {
        if let Some(info) = info {
            self.source_info(info);
        }
    }

    fn opt_keyword(&mut self, keyword: Option<SourceInfo<u32>>) {
        if let Some(keyword) = keyword {
            self.keyword(keyword);
        }
    }

    fn keyword(&mut self, keyword: SourceInfo<u32>) {
        self.source_info(keyword);
        self.buffer.push(' ');
    }

    fn source_info(&mut self, info: SourceInfo<u32>) {
        self.span(info.span);
        self.white_space(info.after(), 0);
    }

    pub fn white_space(&mut self, span: Span, spacing: usize) {
        fn count_tariling_newlines(str: &str) -> usize {
            str.chars().rev().take_while(|c| *c == '\n').count()
        }

        let mut new_lines = count_tariling_newlines(self.buffer);
        let original = self.buffer.len();
        let original_newline = self.last_newline;
        let mut is_usefull = false;
        for (tok, range) in SkippedToken::lexer(&self.source[span.range()]).spanned() {
            match tok {
                SkippedToken::Comment | SkippedToken::MultiComment => {
                    is_usefull = true;
                    if self.at_newline() {
                        self.indent();
                    } else {
                        self.buffer.push(' ');
                    }
                    self.span(Span::new(range).shifted(span.start as usize));
                }
                SkippedToken::NewLine => {
                    if new_lines < self.cfg.max_newlines {
                        self.newline();
                        new_lines += 1;
                        is_usefull |= new_lines > 1;
                    }
                    continue;
                }
                SkippedToken::Space | SkippedToken::Error => continue,
            }

            new_lines = 0;
        }

        if !is_usefull {
            self.buffer.truncate(original);
            self.last_newline = original_newline;
        }

        // we do + 1 to avoid 0 case
        let new_lines = count_tariling_newlines(self.buffer);
        if let Some(to_add) = spacing.checked_sub(new_lines + 1) {
            self.buffer.extend(iter::repeat('\n').take(to_add + 1));
            self.last_newline = self.buffer.len();
        }
    }

    fn fits(&self, len: usize) -> bool {
        self.buffer.len() - self.last_newline + len <= self.cfg.max_line_width
    }

    fn span(&mut self, span: Span) {
        self.buffer.push_str(&self.source[span.range()]);
        self.handle_span_newlines(span);
    }

    fn indent(&mut self) {
        self.buffer.extend(iter::repeat(' ').take(self.indent * 4));
    }

    fn newline_if_none(&mut self) {
        if self.at_newline() {
            return;
        }
        self.newline();
    }

    fn newline(&mut self) {
        self.buffer.push('\n');
        self.last_newline = self.buffer.len();
    }

    fn at_newline(&self) -> bool {
        self.last_newline == self.buffer.len()
    }

    fn handle_span_newlines(&mut self, span: Span) {
        let Some(index) = self.buffer[self.buffer.len() - span.len()..].rfind('\n') else {
            return;
        };

        self.last_newline = self.buffer.len() - span.len() + index;
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

    pub(crate) compressed_lengths: Map<Span, usize>,
}

impl FmtCtx {
    fn dive(&mut self) {
        self.indent += 1;
    }

    fn emerge(&mut self) {
        self.indent -= 1;
    }

    fn clear(&mut self) {
        self.last_newline = 0;
        self.indent = 0;
        self.compressed_lengths.clear();
    }
}

#[derive(Debug, Clone)]
pub struct FmtCfg {
    pub max_line_width: usize,
    pub max_newlines: usize,
    /// if the average length of list item is greater, we distribute
    /// item per line, otherwise we try to fit as many items as possible
    pub max_list_item_length: usize,
    pub tab_width: usize,
}

impl FmtCfg {
    pub const DEFAULT: Self = Self {
        max_line_width: 80,
        max_newlines: 2,
        max_list_item_length: 20,
        tab_width: 4,
    };
}

impl Default for FmtCfg {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Default)]
struct ExprFmtState {
    op_dive: bool,
    dot_dive: bool,
}

impl ExprFmtState {
    fn indent_op(&mut self, fmt: &mut Fmt) {
        fmt.cond_emerge(&mut self.dot_dive);
        fmt.cond_dive_low(&mut self.op_dive);
    }

    fn indent_dot(&mut self, fmt: &mut Fmt) {
        fmt.cond_dive_low(&mut self.dot_dive);
    }

    fn emerge(&mut self, fmt: &mut Fmt) {
        fmt.cond_emerge(&mut self.op_dive);
        fmt.cond_emerge(&mut self.dot_dive);
    }
}

#[derive(Default)]
pub struct SpaceReplacer {
    swap: String,
}

impl SpaceReplacer {
    pub fn replace(&mut self, string: &mut String, tab_width: usize) {
        self.swap.clear();
        for line in string.lines() {
            let Some(space_count) = line.find(|c| c != ' ') else {
                self.swap.push('\n');
                continue;
            };
            let tab_count = space_count / tab_width;
            self.swap.extend(iter::repeat('\t').take(tab_count));
            self.swap.push_str(&line[space_count..]);
            self.swap.push('\n');
        }
        string.clear();
        string.push_str(self.swap.trim_end());
    }
}

#[cfg(test)]
mod test {
    use super::SpaceReplacer;

    #[test]
    fn test_replace_withspace_with_tabs() {
        let mut replacer = SpaceReplacer::default();
        let mut state = String::new();
        let mut check = |input: &str, output: &str| {
            state.clear();
            state.push_str(input);

            replacer.replace(&mut state, 2);

            assert_eq!(state.as_str(), output, "input: {input:?}");
        };

        check("", "");
        check("\n", "");
        check("\n  ", "");
        check("  bang", "\tbang");
        check("\n  hello\n  there\n  ", "\n\thello\n\tthere");
    }
}
