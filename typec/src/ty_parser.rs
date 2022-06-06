use crate::*;

impl TyParser<'_> {
    pub fn subtype(&mut self, parent: Ty, child: Ty) -> Ty {
        // better just assert and force caller to prepare this, in general
        // case this is needed anyway and llvm can optimize this when inlining
        assert_eq!(self.types.base_of(parent), parent);

        if let TyKind::Instance(.., params) = self.types[parent].kind {
            let params = self.ty_lists.get(params).to_vec();
            return self.instantiate(child, &params);
        }

        child
    }

    pub fn instantiate(&mut self, target: Ty, params: &[Ty]) -> Ty {
        let TyEnt {
            kind, flags, name, ..
        } = self.types[target];

        // println!("instantiate: {}", ty_display!(self, target));
        // for &param in params {
        // println!(" param: {}", ty_display!(self, param));
        // }

        if !flags.contains(TyFlags::GENERIC) {
            return target;
        }

        let result = match kind {
            TyKind::Param(i, ..) => params[i as usize],
            TyKind::Ptr(ty, ..) => {
                let ty = self.instantiate(ty, params);
                pointer_of(ty, flags.contains(TyFlags::MUTABLE), self.types, self.ty_instances)
            }
            TyKind::Instance(base, i_params) => {
                self.ty_lists.mark_frame();
                for param in self.ty_lists.get(i_params).to_vec() {
                    // TODO: optimize if needed
                    let ty = self.instantiate(param, params);
                    self.ty_lists.push_one(ty);
                }

                let result = self.parse_instance_type_low(base, name);

                let TyEnt { kind, flags, .. } = self.types[result];
                if !flags.contains(TyFlags::GENERIC) {
                    let TyKind::Instance(base, params) = kind else {
                        unreachable!();
                    };

                    let kind = self.types[base].kind;
                    match kind {
                        TyKind::Struct(ty_comps) => {
                            let params = self.ty_lists.get(params).to_vec(); // TODO: optimize if needed
                            for field in self.ty_comps.get(ty_comps) {
                                self.instantiate(field.ty, &params);
                            }
                        }
                        kind => todo!("{kind:?}"),
                    }
                }

                result
            }
            _ => todo!(),
        };

        result
    }

    pub fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let res = self.parse_type_optional(ty)?;
        if res.is_reserved_value() {
            let span = self.ast_data.nodes[ty].span;
            self.diagnostics
                .push(TyError::ExpectedConcreteType { loc: span });
            return Err(());
        }
        Ok(res)
    }

    /// parse a type just like `parse_type` but can return `Ty::reserved_value` in case the ty is '_'.
    pub fn parse_type_optional(&mut self, ty: Ast) -> errors::Result<Ty> {
        let ast::AstEnt { kind, span, .. } = self.ast_data.nodes[ty];
        match kind {
            AstKind::Ident => self.parse_ident_type(span),
            AstKind::Instantiation => self.parse_instance_type(ty),
            AstKind::Ref(mutable) => self.parse_ptr_type(ty, mutable),
            _ => {
                self.diagnostics
                    .push(TyError::InvalidTypeExpression { loc: span });
                return Err(());
            }
        }
    }

    pub fn parse_ident_type(&mut self, span: Span) -> errors::Result<Ty> {
        let str = self.sources.display(span);

        if str == "_" {
            return Ok(Ty::reserved_value());
        }

        match self.scope.get_concrete::<Ty>(str) {
            Ok(ty) => Ok(ty),
            Err(err) => todo!("{err:?}"),
        }
    }

    fn parse_instance_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let children = self.ast_data.children(ty);
        let header = self.parse_type(children[0])?;

        self.ty_lists.mark_frame();

        for &param in &children[1..] {
            let param = self.parse_type(param)?;
            self.ty_lists.push_one(param);
        }

        Ok(self.parse_instance_type_low(header, self.ast_data.nodes[ty].span))
    }

    /// further specification of [`parse_type`], it expects the `ty` to be of [`ast::Kind::Instantiation`], if instance already exists, it is reused.
    pub fn parse_instance_type_low(&mut self, header: Ty, span: Span) -> Ty {
        let mut id = ID::new("<instance>") + self.types[header].id;
        let mut generic = false;

        for &param in self.ty_lists.top() {
            id = id + self.types[param].id;
            generic |= self.types[param].flags.contains(TyFlags::GENERIC);
        }

        if let Some(&already) = self.ty_instances.get(id) {
            self.ty_lists.discard();
            return already;
        }

        let params = self.ty_lists.pop_frame();

        let result = {
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Instance(header, params),
                flags: TyFlags::GENERIC & generic,
            };
            self.types.push(ent)
        };

        self.ty_instances.insert_unique(id, result);

        result
    }

    pub fn parse_ptr_type(&mut self, ty: Ast, mutable: bool) -> errors::Result<Ty> {
        let inner_ty = {
            let inner = self.ast_data.children(ty)[0];
            self.parse_type(inner)?
        };

        Ok(pointer_of(inner_ty, mutable, self.types, self.ty_instances))
    }

    pub fn parse_composite_bound(&mut self, asts: &[Ast], span: Span) -> Ty {
        self.ty_lists.mark_frame();
        for &bound in asts {
            let Ok(ty) = self.parse_type(bound) else {
                continue;
            };
            self.ty_lists.push_one(ty);
        }
        self.parse_composite_bound_low(span)
    }

    pub fn parse_composite_bound_low(&mut self, span: Span) -> Ty {
        self.ty_lists.top_mut().sort_by_key(|ty| ty.0);
        let duplicates = self.ty_lists.top().windows(2).any(|w| w[0] == w[1]);

        if duplicates {
            self.diagnostics.push(TyError::DuplicateBound { loc: span });
        }

        let base_id = self.types[self.builtin_types.any].id;

        let id = self
            .ty_lists
            .top()
            .iter()
            .map(|&ty| self.types[ty].id)
            .fold(base_id, |acc, id| acc + id);

        if let Some(&already) = self.ty_instances.get(id) {
            self.ty_lists.discard();
            return already;
        }

        // make bound combo implement all contained bounds
        for &ty in self.ty_lists.top() {
            let TyKind::Bound(funcs) = self.types[ty].kind else {
                unreachable!();
            };
            let bound = self.types[ty].id;
            let id = ID::bound_impl(bound, id);
            let bound = BoundImpl {
                span: Default::default(),
                funcs,
            };
            self.bound_impls.insert_unique(id, bound);
        }

        let combo = {
            let bounds = self.ty_lists.pop_frame();
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Param(0, bounds, None.into()),
                flags: TyFlags::GENERIC,
            };
            self.types.push(ent)
        };

        self.ty_instances.insert_unique(id, combo);

        combo
    }

    pub fn func_pointer_of(&mut self, sig: Sig, generic: bool) -> Ty {
        let id = self.id_of_sig(sig);

        if let Some(&already) = self.ty_instances.get(id) {
            return already;
        }

        let ty_ent = TyEnt {
            id,
            name: Span::new(self.types[sig.ret].name.source(), 0, 0),
            kind: TyKind::FuncPtr(sig),
            flags: TyFlags::GENERIC & generic,
        };
        let ty = self.types.push(ty_ent);
        self.ty_instances.insert_unique(id, ty);

        ty
    }

    pub fn id_of_sig(&self, sig: Sig) -> ID {
        let mut id = ID::new("<func_pointer>");

        id = id + ID::new("<args>");

        for &arg in self.ty_lists.get(sig.args) {
            let arg = self.types[arg].id;
            id = id + arg;
        }

        id = id + ID::new("<ret>");

        let ret = self.types[sig.ret].id;
        id + ret
    }
}
