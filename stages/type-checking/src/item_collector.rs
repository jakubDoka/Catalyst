use crate::*;
use diags::inner_lexing::Span;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl ItemCollector<'_> {
    pub fn collect(&mut self, ast: Maybe<AstList>, ctx: &mut ItemContext) -> errors::Result {
        for &item in &self.ast_data[ast] {
            let res = match item.kind {
                AstKind::Bound { vis } => self.collect_bound(item, vis, ctx),
                AstKind::Struct { vis } => self.collect_struct(item, vis, ctx),
                AstKind::Func { .. } | AstKind::Impl { .. } | AstKind::BoundImpl { .. } => continue,
                kind => unimplemented!("{:?}", kind),
            };

            let Ok(Some(res)) = res else {
                continue;
            };

            insert_scope_item!(self, res);
        }

        for &item in &self.ast_data[ast] {
            let res = match item.kind {
                AstKind::Struct { .. } | AstKind::Impl { .. } | AstKind::Bound { .. } => continue,

                AstKind::BoundImpl { .. } => self.collect_bound_impl(item, ctx),
                AstKind::Func { vis } => self.collect_fn(item, vis, ctx),
                kind => unimplemented!("{:?}", kind),
            };

            let Ok(Some(res)) = res else {
                continue;
            };

            insert_scope_item!(self, res);
        }

        Ok(())
    }

    fn collect_bound_impl(
        &mut self,
        item: AstEnt,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, bound, target, ..] = self.ast_data[item.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let bound = ty_parser!(self, self.current_file).parse(bound)?;
        let implementor = ty_parser!(self, self.current_file).parse(target)?;
        self.scope.end_frame();

        let id = {
            let bound_base = self.types.instance_base_of(bound);
            let implementor_base = self.types.instance_base_of(implementor);
            self.interner.intern(bound_impl_ident!(
                self.types.ents.id(bound_base),
                self.types.ents.id(implementor_base)
            ))
        };

        let next = self.types.impl_index.insert(id, self.types.impls.next());

        let impl_ent = ImplEnt {
            params,
            bound,
            implementor,
            span: target.span.into(),
            next: next.into(),
        };
        let r#impl = self.types.impls.push(impl_ent);
        ctx.impls.push((item, r#impl));

        Ok(None)
    }

    fn collect_fn(
        &mut self,
        item: AstEnt,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [cc, generics, name, ref args @ .., ret, _body] = self.ast_data[item.children] else {
            unreachable!("{:?}", &self.ast_data[item.children]);
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;

        let (local_id, id) = self.compute_ids(name.span, vis);

        let ent = DefEnt {
            true_func: id,
            source: self.current_file.into(),
            span: name.span.into(),
            body: Maybe::none(),
            tir_data: TirData::new(),
            sig,
        };
        let def = self.funcs.defs.push(ent);
        let ent = FuncEnt {
            params,
            flags: FuncFlags::GENERIC & params.is_some(),
            def,
        };
        self.funcs.ents.insert(id, ent);
        ctx.funcs.push((item, def));

        Ok(Some(ModItem::new(local_id, def, name.span)))
    }

    fn collect_bound(
        &mut self,
        item: AstEnt,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, name, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        let (local_id, id) = self.compute_ids(name.span, vis);

        let assoc_types = ty_parser!(self, self.current_file).assoc_types(body, id);

        let ent = TyEnt {
            kind: TyKind::Bound {
                inherits: Maybe::none(),
                assoc_types,
                funcs: Maybe::none(),
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8
                + self.types.slices[assoc_types].len() as u8,
            file: self.current_file.into(),
            span: name.span.into(),
        };
        let ty = self.types.ents.insert_unique(id, ent);
        ctx.types.push((item, ty));

        Ok(Some(ModItem::new(local_id, ty, name.span)))
    }

    fn collect_struct(
        &mut self,
        item: AstEnt,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, name, ..] = self.ast_data[item.children] else {
            unreachable!();
        };

        let (local_id, id) = self.compute_ids(name.span, vis);

        let ent = TyEnt {
            kind: TyKind::Inferrable,
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8,
            file: self.current_file.into(),
            span: name.span.into(),
        };
        let ty = self.types.ents.insert_unique(id, ent);
        ctx.types.push((item, ty));

        Ok(Some(ModItem::new(local_id, ty, name.span)))
    }

    fn compute_ids(&mut self, name: Span, vis: Vis) -> (Ident, Ident) {
        let local = self.interner.intern_str(span_str!(self, name));
        let id = intern_scoped_ident!(self, local);
        self.visibility[id] = vis;
        (local, id)
    }
}
