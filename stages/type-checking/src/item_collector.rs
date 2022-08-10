use crate::*;
use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl ItemCollector<'_> {
    pub fn collect(&mut self, ast: Maybe<AstList>, ctx: &mut ItemContext) {
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

            self.insert_scope_item(res);
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

            self.insert_scope_item(res);
        }
    }

    fn collect_bound_impl(
        &mut self,
        item: AstEnt,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, ast_bound, ast_implementor, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let mut bound = ty_parser!(self, self.current_file).parse_impl_bound(ast_bound)?;
        let base_bound = self.typec.instance_base_of(bound);
        let implementor = ty_parser!(self, self.current_file).parse(ast_implementor)?;
        self.scope.end_frame();

        let mut changed = false;
        for &item in &self.ast_data[body.children] {
            if item.kind != AstKind::ImplType {
                continue;
            };

            let [_generics, ast_name, ast_value] = self.ast_data[item.children] else {
                unreachable!();
            };

            let value = ty_parser!(self, self.current_file).parse(ast_value)?;
            let name = self.interner.intern_str(span_str!(self, ast_name.span));
            let Some(index) = self.typec.bound_assoc_ty_index(base_bound, name) else {
                let loc = self.typec.loc_of(base_bound, self.interner);
                self.workspace.push(diag! {
                    (ast_name.span, self.current_file) => "no such associated type on the bound",
                    (exp loc) => "related bound defined here",
                });
                return Err(());
            };

            let assoc_ty_offset =
                self.typec.param_count(base_bound) - self.typec.assoc_ty_count_of_bound(base_bound);
            self.typec
                .set_instance_param_on(bound, index + assoc_ty_offset, value);
            changed = true;
        }

        if let TyKind::Instance { base, params, .. } = self.typec.types[bound].kind {
            let mut iter = self
                .typec
                .ty_lists
                .get(params)
                .iter()
                .skip(self.typec.param_count(base) - self.typec.assoc_ty_count_of_bound(base))
                .enumerate()
                .filter_map(|(i, &param)| (param == BuiltinTypes::INFERRED).then_some(i))
                .peekable();

            if iter.peek().is_some() {
                let list = iter
                    .map(|i| self.typec.bound_assoc_ty_at(base, i))
                    .map(|assoc_ty| self.typec.types[assoc_ty].loc.name)
                    .map(|ident| &self.interner[ident])
                    .collect::<Vec<_>>()
                    .join(", ");

                let loc = self.typec.loc_of(base, self.interner);

                self.workspace.push(diag! {
                    (item.span, self.current_file) => "not all associated types are specified",
                    (none) => "missing: {}" { list },
                    (exp loc) => "related bound defined here",
                });
                return Err(());
            }
        }

        if changed {
            bound = ty_factory!(self).rehash_ty(bound);
        }

        if !self.typec.is_valid_bound(bound) {
            self.workspace.push(diag! {
                (ast_bound.span, self.current_file) => "expected valid bound type"
            });
        }

        if self.typec.has_param_base(implementor) {
            self.workspace.push(diag! {
                (ast_implementor.span, self.current_file) => "expected valid implementor type",
                (none) => "implementor cannot have generic base",
                (none) => "if you want to implement bound for a bound use it explicitly",
            });
        }

        let id = {
            let bound_base = self.typec.instance_base_of(bound);
            let implementor_base = self.typec.instance_base_of(implementor);
            self.interner.intern(bound_impl_ident!(
                self.typec.types.id(bound_base),
                self.typec.types.id(implementor_base)
            ))
        };

        let next = self.typec.impls.index(id);
        let impl_ent = ImplEnt {
            id,
            params,
            bound,
            implementor,
            funcs: Maybe::none(),
            loc: Loc::new(
                item.span,
                self.current_file,
                self.interner.intern_str("impl"),
            ),
            next: next.into(),
        };
        let r#impl = self.typec.impls.redirect_insert(id, impl_ent);
        ctx.bound_impls.push((item, r#impl));

        let mut current = next;
        while let Some(other) = current {
            if bound_checker!(self).impls_overlap(r#impl, other) {
                let loc = self.typec.loc_of_impl(other, self.interner);
                self.workspace.push(diag! {
                    (ast_implementor.span, self.current_file) => "implementation overlaps with existing one",
                    (exp loc) => "colliding implementation",
                })
            }
            current = self.typec.impls[other].next.expand();
        }

        Ok(None)
    }

    fn collect_fn(
        &mut self,
        item: AstEnt,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [cc, generics, ast_name, ref args @ .., ret, _body] = self.ast_data[item.children] else {
            unreachable!("{:?}", &self.ast_data[item.children]);
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;

        let (local_id, id) = self.compute_ids(ast_name.span, vis);
        let name = span_str!(self, ast_name.span);

        let ent = DefEnt {
            params,
            flags: FuncFlags::GENERIC & params.is_some(),
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
            body: Maybe::none(),
            tir_data: TirData::new(),
            sig,
        };
        let def = self.typec.defs.insert_unique(id, ent);
        ctx.funcs.push((item, def));

        Ok(Some(ModItem::new(local_id, def, ast_name.span)))
    }

    fn collect_bound(
        &mut self,
        item: AstEnt,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, ast_name, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        let (local_id, id) = self.compute_ids(ast_name.span, vis);

        let assoc_types = ty_parser!(self, self.current_file).assoc_types(body, id, local_id);

        let name = span_str!(self, ast_name.span);
        let ent = TyEnt {
            kind: TyKind::Bound {
                inherits: Maybe::none(),
                assoc_types,
                funcs: Maybe::none(),
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8
                + self.typec.ty_lists[assoc_types].len() as u8,
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
        };
        let ty = self.typec.types.insert_unique(id, ent);
        ctx.types.push((item, ty));

        Ok(Some(ModItem::new(local_id, ty, ast_name.span)))
    }

    fn collect_struct(
        &mut self,
        item: AstEnt,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, ast_name, ..] = self.ast_data[item.children] else {
            unreachable!();
        };

        let (local_id, id) = self.compute_ids(ast_name.span, vis);
        let name = span_str!(self, ast_name.span);

        let ent = TyEnt {
            kind: TyKind::Inferrable,
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8,
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
        };
        let ty = self.typec.types.insert_unique(id, ent);
        ctx.types.push((item, ty));

        Ok(Some(ModItem::new(local_id, ty, ast_name.span)))
    }

    fn compute_ids(&mut self, name: Span, vis: Vis) -> (Ident, Ident) {
        let local = self.interner.intern_str(span_str!(self, name));
        let id = intern_scoped_ident!(self, local);
        self.visibility[id] = vis;
        (local, id)
    }

    insert_scope_item!();
}
