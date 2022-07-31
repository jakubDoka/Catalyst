use std::ops::Not;

use crate::*;
use diags::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl TyParser<'_> {
    pub fn parse_bound_sum(&mut self, ast: &[AstEnt]) -> errors::Result<Ty> {
        let bounds = ast
            .iter()
            .filter_map(|&child| self.parse(child).ok())
            .collect::<Vec<_>>();

        if bounds.len() != ast.len() {
            return Err(());
        }

        if bounds.len() == 1 {
            Ok(bounds[0])
        } else {
            Ok(ty_factory!(self).anon_bound_of(&bounds))
        }
    }

    pub fn parse(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        match ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.parse_ident(ast),
            AstKind::PtrTy { mutable } => self.parse_ptr_ty(mutable, ast),
            AstKind::TyInstance => self.parse_instance(ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn parse_ident(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        let id = self.ident_chain_id(ast);
        self.scope
            .get_concrete::<Ty>(id)
            .map_err(scope_error_handler!(self, ast.span, id, "type not found"))
    }

    fn parse_ptr_ty(&mut self, mutable: bool, ast: AstEnt) -> errors::Result<Ty> {
        let base = self.parse(self.ast_data[ast.children][0])?;
        Ok(ty_factory!(self).pointer_of(mutable, base))
    }

    fn parse_filed_ty(&mut self, base: Ty, ast: AstEnt) -> errors::Result<(Ty, usize)> {
        let [name, ty] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let ty = self.parse(ty)?;

        let id = self.interner.intern(scoped_ident!(
            self.typec.types.id(base),
            span_str!(self, name.span)
        ));

        let assoc_ty = self.typec.types.index(id).ok_or_else(|| {
            self.workspace.push(diag! {
                (name.span, self.current_file) => "associated type not found"
            })
        })?;
        let index = self.typec.assoc_ty_index(assoc_ty).ok_or_else(|| {
            self.workspace.push(diag! {
                (name.span, self.current_file) => "this is not an associated type"
            })
        })?;

        Ok((ty, index + 1))
    }

    fn parse_instance(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        let (&first, rest) = self.ast_data[ast.children].split_first().unwrap();

        let base = self.parse(first)?;

        let mut params = Vec::with_capacity(rest.len());
        params.extend(rest.iter().filter_map(|&child| {
            if child.kind != AstKind::FieldTy {
                self.parse(child).ok().map(|ty| (ty, 0))
            } else {
                self.parse_filed_ty(base, child).ok()
            }
        }));
        if params.len() != rest.len() {
            return Err(());
        }

        let param_count = self.typec.param_count(base);
        if params.len() != param_count {
            self.workspace.push(diag! {
                (ast.span, self.current_file) => "wrong number of type parameters",
                (none) => "expected {}, got {}" { param_count, params.len() },
            });
            return Err(());
        }

        params.sort_by_key(|&(_, index)| index);

        Ok(ty_factory!(self).instance(base, params.into_iter().map(|(ty, _)| ty)))
    }

    pub fn sig(&mut self, cc: AstEnt, args: &[AstEnt], ret: AstEnt) -> errors::Result<Sig> {
        let cc = cc
            .kind
            .is_none()
            .not()
            .then(|| self.interner.intern_str(span_str!(self, cc.span.shrink(1))))
            .into();
        let args = self.args(args)?;
        let ret = if ret.kind.is_none() {
            Maybe::none()
        } else {
            ty_parser!(self, self.current_file).parse(ret)?.into()
        };
        Ok(Sig { cc, args, ret })
    }

    pub fn args(&mut self, args: &[AstEnt]) -> errors::Result<Maybe<TyList>> {
        let mut reserved = self.typec.slices.reserve(args.len());
        for &ast_arg in args {
            let [.., ty] = self.ast_data[ast_arg.children] else {
                unreachable!("{:?}", &self.ast_data[ast_arg.children]);
            };
            let Ok(ty) = ty_parser!(self, self.current_file).parse(ty) else {
                continue;
            };
            self.typec.slices.push_to_reserved(&mut reserved, ty);
        }
        Ok(self.typec.slices.fill_reserved(reserved, BuiltinTypes::ANY))
    }

    pub fn bounded_generics(&mut self, generics: AstEnt) -> errors::Result<Maybe<TyList>> {
        let mut params = Vec::with_capacity(self.ast_data[generics.children].len());
        for &ast_param in &self.ast_data[generics.children] {
            let (&name, bounds) = self.ast_data[ast_param.children].split_first().unwrap();
            let name = self.interner.intern_str(span_str!(self, name.span));

            let bound = ty_parser!(self, self.current_file).parse_bound_sum(bounds)?;
            let param = ty_factory!(self).param_of(bound);
            let item = ScopeItem::new(name, param, ast_param.span, self.current_file);
            self.scope.push(item);

            // handle duplicate, all params need to be unique
            let param = params
                .iter()
                .rev()
                .find_map(|&p| (p == param).then(|| ty_factory!(self).next_param_of(param)))
                .unwrap_or(param);

            params.push(param);
        }
        Ok(self.typec.slices.bump_slice(&params))
    }

    pub fn generics(&mut self, generics: AstEnt) {
        let mut param = BuiltinTypes::TY_ANY;
        for &(mut ast_param) in &self.ast_data[generics.children] {
            if ast_param.kind != AstKind::Ident {
                ast_param = self.ast_data[ast_param.children][0];
            }
            let id = self.interner.intern_str(span_str!(self, ast_param.span));
            let item = ScopeItem {
                id,
                ptr: ScopePtr::new(param),
                span: ast_param.span,
                module: self.current_file,
            };
            self.scope.push(item);
            param = ty_factory!(self).next_param_of(param);
        }
    }

    pub fn assoc_types(&mut self, ast: AstEnt, bound_id: Ident) -> Maybe<TyList> {
        let assoc_type_count = self.ast_data[ast.children]
            .iter()
            .filter(|item| matches!(item.kind, AstKind::BoundType { .. }))
            .count();

        let mut assoc_types = self.typec.slices.reserve(assoc_type_count);

        for &item in self.ast_data[ast.children].iter() {
            let AstKind::BoundType { vis } = item.kind else {
                continue;
            };

            drop(self.assoc_type(item, bound_id, &mut assoc_types, vis));
        }

        self.typec
            .slices
            .fill_reserved(assoc_types, BuiltinTypes::ANY)
    }

    fn assoc_type(
        &mut self,
        ast: AstEnt,
        bound_id: Ident,
        assoc_types: &mut Reserved<TyList>,
        vis: Vis,
    ) -> errors::Result {
        let &[generics, name] = &self.ast_data[ast.children] else {
            unreachable!("{:?}", &self.ast_data[ast.children]);
        };

        let id = self
            .interner
            .intern(scoped_ident!(bound_id, span_str!(self, name.span)));
        self.visibility[id] = vis;

        if let Some(prev) = self.typec.types.get(id) {
            duplicate_definition!(self, ast.span, prev.span);
            return Err(());
        }

        let ty_ent = TyEnt {
            kind: TyKind::AssocType {
                index: self.typec.slices.reserve_len(&assoc_types) as u32,
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8,
            file: self.current_file.into(),
            span: name.span.into(),
        };
        let ty = self.typec.types.insert_unique(id, ty_ent);
        self.typec.slices.push_to_reserved(assoc_types, ty);

        Ok(())
    }

    pub fn ident_chain_id(&mut self, ast: AstEnt) -> Ident {
        match ast.kind {
            AstKind::IdentChain => {
                let segments = self.ast_data[ast.children]
                    .iter()
                    .map(|child| self.packages.span_str(self.current_file, child.span))
                    .map(|str| self.interner.intern_str(str))
                    .map(|id| self.scope.get_concrete::<Ident>(id).unwrap_or(id))
                    .map(|segment| [InternedSegment::from(segment), "`".into()])
                    .flatten()
                    .take(
                        (self.ast_data[ast.children].len() * 2)
                            .checked_sub(1)
                            .unwrap_or(0),
                    ) // -1 for the excess `
                    .collect::<Vec<_>>();

                self.interner.intern(&segments)
            }
            AstKind::Ident => {
                let str = self.packages.span_str(self.current_file, ast.span);
                let id = self.interner.intern_str(str);
                self.scope.get_concrete::<Ident>(id).unwrap_or(id)
            }
            _ => unimplemented!(),
        }
    }
}
