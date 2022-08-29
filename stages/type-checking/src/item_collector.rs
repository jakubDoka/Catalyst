use std::{default::default, vec};

use crate::*;
use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl ItemCollector<'_> {
    pub fn collect(&mut self, ast: VSlice<Ast>, ctx: &mut ItemContext) {
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
                AstKind::Struct { .. } | AstKind::Bound { .. } => continue,
                AstKind::Impl { vis } => {
                    drop(self.collect_impl(item, vis, ctx));
                    continue;
                }
                AstKind::BoundImpl { .. } => self.collect_bound_impl(item, ctx),
                AstKind::Func { vis } => self.collect_fn(item, vis, default(), ctx),
                kind => unimplemented!("{:?}", kind),
            };

            let Ok(Some(res)) = res else {
                continue;
            };

            self.insert_scope_item(res);
        }
    }

    fn collect_impl(&mut self, ast: Ast, vis: Vis, ctx: &mut ItemContext) -> errors::Result {
        let [ast_generics, ast_type, ast_body] = self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let generics = ty_parser!(self, self.current_file).bounded_generics(ast_generics)?;

        let ty = ty_parser!(self, self.current_file).parse(ast_type)?;
        let name = self.typec.types[ty].loc.name;
        self.scope.self_alias = name.into();

        for &func in &self.ast_data[ast_body.children] {
            let AstKind::Func { vis: fn_vis } = func.kind else {
                unreachable!();
            };
            let vis = vis.merge(fn_vis);

            let Ok(Some(res)) = self.collect_fn(func, vis, generics, ctx) else {
                continue;
            };
            self.insert_scope_item(res);
        }

        self.scope.self_alias.take();
        self.scope.end_frame();

        Ok(())
    }

    fn collect_bound_impl(
        &mut self,
        item: Ast,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, ast_bound, ast_implementor, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let mut bound = ty_parser!(self, self.current_file).parse_impl_bound(ast_bound)?;
        let base_bound = self.typec.instance_base(bound);
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

        if let TyKind::Instance { base, .. } = self.typec.types[bound].kind {
            let mut iter = self
                .typec
                .params_of_ty(bound)
                .iter()
                .skip(self.typec.param_count(base) - self.typec.assoc_ty_count_of_bound(base))
                .enumerate()
                .filter_map(|(i, &param)| (param == Ty::INFERRED).then_some(i))
                .peekable();

            if iter.peek().is_some() {
                let list = iter
                    .map(|i| self.typec.bound_assoc_ty_at(base, i))
                    .map(|assoc_ty| self.typec.types[assoc_ty].loc.name)
                    .map(|ident| &self.interner[ident])
                    .collect::<BumpVec<_>>()
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

        let evidence_id = {
            let bound_base = self.typec.instance_base(bound);
            let implementor_base = self.typec.instance_base(implementor);
            IdentPair(
                self.typec.types.id(bound_base),
                self.typec.types.id(implementor_base),
            )
        };

        let next = self.typec.impls.index(evidence_id);
        let impl_ent = Impl {
            id: self.interner.intern(impl_pair_ident!(IdentPair(
                self.typec.types.id(bound),
                self.typec.types.id(implementor),
            ))),
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
        let r#impl = self.typec.impls.redirect_insert(evidence_id, impl_ent);
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
        item: Ast,
        vis: Vis,
        upper_params: VSlice<VRef<Bound>>,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [cc, generics, ast_name, ref args @ .., ret, _body] = self.ast_data[item.children] else {
            unreachable!("{:?}", &self.ast_data[item.children]);
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;

        let (local_id, _) = self.compute_ids(ast_name.span);
        let name = span_str!(self, ast_name.span);

        let ent = Def {
            generics: params,
            flags: DefFlags::GENERIC & params.is_some() & DefFlags::from(vis),
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
            body: Maybe::none(),
            tir_data: TirData::new(),
            sig,
            upper_generics: upper_params,
        };
        let def = self.typec.defs.push(ent);
        ctx.funcs.push((self.scope.self_alias, item, def));

        Ok(Some(ModItem::new(local_id, def, ast_name.span, vis)))
    }

    fn collect_bound(
        &mut self,
        item: Ast,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, ast_name, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        let mut param_buffer = bumpvec![];
        ty_parser!(self, self.current_file).bounded_generics_low(generics, &mut param_buffer)?;

        let (local_id, id) = self.compute_ids(ast_name.span);

        let assoc_type_count =
            ty_parser!(self, self.current_file).assoc_types(body, id, local_id, &mut param_buffer);

        let name = span_str!(self, ast_name.span);
        let ent = Ty {
            kind: TyKind::SelfBound {
                inherits: Maybe::none(),
                assoc_type_count: assoc_type_count as u32,
                funcs: Maybe::none(),
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            params: self.typec.ty_lists.bump(param_buffer),
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
        };
        let ty = self.typec.types.insert_unique(id, ent);
        ctx.types.push((item, ty));

        Ok(Some(ModItem::new(local_id, ty, ast_name.span, vis)))
    }

    fn collect_struct(
        &mut self,
        item: Ast,
        vis: Vis,
        ctx: &mut ItemContext,
    ) -> errors::Result<Option<ModItem>> {
        let [generics, ast_name, ..] = self.ast_data[item.children] else {
            unreachable!();
        };

        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;

        let (local_id, id) = self.compute_ids(ast_name.span);
        let name = span_str!(self, ast_name.span);

        let ent = Ty {
            kind: TyKind::Inferrable,
            flags: TyFlags::GENERIC & generics.children.is_some(),
            params,
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
        };
        let ty = self.typec.types.insert_unique(id, ent);
        ctx.types.push((item, ty));

        Ok(Some(ModItem::new(local_id, ty, ast_name.span, vis)))
    }

    fn compute_ids(&mut self, name: Span) -> (Ident, Ident) {
        let name_str = span_str!(self, name);
        let local = if let Some(self_alias) = self.scope.self_alias.expand() {
            self.interner.intern(scoped_ident!(self_alias, name_str))
        } else {
            self.interner.intern_str(name_str)
        };
        let id = intern_scoped_ident!(self, local);
        (local, id)
    }

    insert_scope_item!();
}
