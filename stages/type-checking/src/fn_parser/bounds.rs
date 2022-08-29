use super::*;

impl FuncParser<'_> {
    pub fn bound_impls(&mut self, impls: impl IntoIterator<Item = (Ast, VRef<Impl>)>) {
        for (ast, r#impl) in impls {
            let [generics, .., body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            let unwrapped_impl = self.unwrap_impl(r#impl);

            self.scope.start_frame();
            self.scope.self_alias = self.typec.types[unwrapped_impl.ent.implementor]
                .loc
                .name
                .into();
            ty_parser!(self, self.current_file).push_generics(generics, unwrapped_impl.ent.params);

            for &item in &self.ast_data[body.children] {
                match item.kind {
                    AstKind::ImplType => drop(self.impl_type(item, unwrapped_impl)),
                    AstKind::ImplUse | AstKind::Func { .. } => (),
                    kind => unimplemented!("{:?}", kind),
                }
            }

            let func_count = self.typec.bound_funcs[unwrapped_impl.base.funcs].len();
            let mut funcs = bumpvec![Maybe::none(); func_count];

            for &item in &self.ast_data[body.children] {
                match item.kind {
                    AstKind::Func { .. } => {
                        drop(self.bound_impl_func(item, unwrapped_impl, &mut funcs))
                    }
                    AstKind::ImplUse => drop(self.bound_impl_use(item, unwrapped_impl, &mut funcs)),
                    AstKind::ImplType => (),
                    kind => unimplemented!("{:?}", kind),
                }
            }

            self.scope.end_frame();

            if self.handle_missing_funcs_in_bound_impl(ast, unwrapped_impl, &mut funcs) {
                continue;
            }

            self.typec.impls[r#impl].funcs = self
                .typec
                .func_lists
                .bump(funcs.into_iter().map(Maybe::unwrap));
        }
    }

    fn handle_missing_funcs_in_bound_impl(
        &mut self,
        ast: Ast,
        unwrapped_impl: UnwrappedImpl,
        funcs: &mut [Maybe<VRef<Func>>],
    ) -> bool {
        let mut missing = funcs
            .iter_mut()
            .enumerate()
            .filter(|(.., maybe)| maybe.is_none())
            .filter_map(|(i, maybe)| {
                self.try_recover_missing_func_in_bound_impl(unwrapped_impl, i)
                    .map(|func| *maybe = func.into())
                    .err()
            })
            .peekable();

        if missing.peek().is_none() {
            return false;
        }

        let suggestions = missing
            .collect::<BumpVec<_>>()
            .into_iter()
            .map(|name| &self.interner[name])
            .collect::<BumpVec<_>>()
            .join(", ");

        let loc = self.typec.loc_of(unwrapped_impl.ent.bound, self.interner);
        self.workspace.push(diag! {
            (ast.span, self.current_file) => "some of the methods are not implemented",
            (none) => "missing methods: {}" { suggestions },
            (exp loc) => "related bound type",
        });

        true
    }

    fn try_recover_missing_func_in_bound_impl(
        &mut self,
        unwrapped_impl: UnwrappedImpl,
        func_index: usize,
    ) -> Result<VRef<Func>, Ident> {
        let name = self.typec.bound_funcs[unwrapped_impl.base.funcs][func_index]
            .loc
            .name;

        let implementor_name = self.typec.types[unwrapped_impl.ent.implementor].loc.name;
        let local_id = self.interner.intern(scoped_ident!(implementor_name, name));
        let Ok((def, ..)) = self.scope.get_concrete::<Def>(local_id) else {
            return Err(name);
        };

        if bound_checker!(self)
            .compare_bound_signatures(unwrapped_impl.id, func_index, def)
            .is_err()
        {
            return Err(name);
        }

        let id = self
            .interner
            .intern(scoped_ident!(unwrapped_impl.ent.id, name));
        Ok(self.typec.wrap_def(id, def))
    }

    fn bound_impl_use(
        &mut self,
        ast: Ast,
        unwrapped_impl: UnwrappedImpl,
        funcs: &mut [Maybe<VRef<Func>>],
    ) -> errors::Result {
        let &[value, ast_name] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        let name = span_str!(self, ast_name.span);
        let name_ident = self.interner.intern_str(name);

        let local_id = ty_parser!(self, self.current_file).ident_chain_id(value);
        let def: VRef<Def> =
            self.get_from_scope_concrete(local_id, value.span, "function", Reports::base)?;

        self.add_bound_impl_func(name_ident, def, unwrapped_impl, funcs, ast_name.span)
    }

    fn add_bound_impl_func(
        &mut self,
        name: Ident,
        def: VRef<Def>,
        unwrapped_impl: UnwrappedImpl,
        funcs: &mut [Maybe<VRef<Func>>],
        span: Span,
    ) -> errors::Result {
        let id = self
            .interner
            .intern(scoped_ident!(unwrapped_impl.ent.id, name));
        let func = self.typec.wrap_def(id, def);

        let pos = self.typec.bound_lists[unwrapped_impl.base.assoc_types]
            .iter()
            .position(|&id| self.typec.bounds[id].loc.name == name);

        let Some(index) = pos else {
            let loc = self.typec.loc_of(unwrapped_impl.ent.bound, self.interner);
            self.workspace.push(diag! {
                (span, self.current_file) => "function with this name does not exist for the bound",
                (exp loc) => "related bound defined here"
            });
            return Err(());
        };

        if let Some(already) = funcs[index].expand() {
            let def = self.typec.funcs[already].def;
            let loc = self.typec.loc_of(def, self.interner);
            self.workspace.push(diag! {
                (span, self.current_file) => "function with this name is already implemented",
                (exp loc) => "the function",
            });
            return Err(());
        }

        if let Err(err) =
            bound_checker!(self).compare_bound_signatures(unwrapped_impl.id, index, def)
        {
            let diag = bound_checker!(self).signature_error_to_diag(
                err,
                span,
                self.current_file,
                unwrapped_impl.id,
            );
            self.workspace.push(diag);
            return Err(());
        }

        funcs[index] = Maybe::some(func);

        Ok(())
    }

    fn impl_type(&mut self, ast: Ast, unwrapped_impl: UnwrappedImpl) {
        let [_generics, ast_name, ..] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let name = span_str!(self, ast_name.span);
        let name_ident = self.interner.intern_str(name);
        let implementor_name = self.typec.types[unwrapped_impl.ent.implementor].loc.name;
        let local_id = self
            .interner
            .intern(scoped_ident!(implementor_name, name_ident));

        let ty = self.typec.bound_lists[unwrapped_impl.base.assoc_types]
            .iter()
            .find_map(|&id| (self.typec.bounds[id].loc.name == local_id).then_some(id))
            .expect("this code should not be reachable if the bound is malformed");

        self.scope.push(ScopeItem::new(
            local_id,
            ty,
            ast_name.span,
            self.current_file,
            Vis::Pub,
        ))
    }

    fn bound_impl_func(
        &mut self,
        item: Ast,
        unwrapped_impl: UnwrappedImpl,
        funcs: &mut [Maybe<VRef<Func>>],
    ) -> errors::Result {
        let [cc, generics, ast_name, ref args @ .., ret, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;

        let name = span_str!(self, ast_name.span);
        let def_ent = Def {
            generics: params,
            flags: DefFlags::GENERIC & !params.is_empty(),
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
            sig,
            upper_generics: unwrapped_impl.ent.params,
            ..default()
        };
        let def = self.typec.defs.push(def_ent);

        let name_ident = self.interner.intern_str(name);

        self.add_bound_impl_func(name_ident, def, unwrapped_impl, funcs, ast_name.span)?;

        self.tir_data.clear();
        self.func_parser_ctx.current_fn = def.into();
        let body = self.r#fn(body, generics, args, self.typec.defs[def].sig.ret.expand())?;
        self.typec.defs[def].tir_data = self.tir_data.clone();
        self.typec.defs[def].body = body.into();

        Ok(())
    }
}
