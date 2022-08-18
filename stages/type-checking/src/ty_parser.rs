use std::ops::Not;

use crate::*;
use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl TyParser<'_> {
    pub fn parse_impl_bound(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        let mut maybe_base = self.parse(ast)?;
        if self.typec.instance_base_of(maybe_base) == maybe_base
            && self.typec.param_count(maybe_base) != 0
        {
            maybe_base = self.parse_instance_low(maybe_base, &[], ast.span)?;
        }
        Ok(maybe_base)
    }

    pub fn parse_bound_sum(&mut self, ast: &[AstEnt]) -> errors::Result<Ty> {
        let bounds = ast
            .iter()
            .filter_map(|&child| self.parse(child).ok())
            .collect::<Vec<_>>();

        if bounds.len() != ast.len() {
            return Err(());
        }

        Ok(match bounds.len() {
            0 => BuiltinTypes::ANY,
            1 => bounds[0],
            _ => ty_factory!(self).anon_bound_of(&bounds),
        })
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
        self.get_from_scope_concrete(id, ast.span, "type", Reports::base)
    }

    scope_error_handler!(concrete);

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

        Ok((ty, index))
    }

    fn parse_instance(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        let (&first, rest) = self.ast_data[ast.children].split_first().unwrap();

        let base = self.parse(first)?;

        self.parse_instance_low(base, rest, ast.span)
    }

    pub fn parse_instance_low(
        &mut self,
        base: Ty,
        rest: &[AstEnt],
        span: Span,
    ) -> errors::Result<Ty> {
        let param_count = self.typec.param_count(base);
        let assoc_offset = param_count - self.typec.assoc_ty_count_of_bound(base);

        let mut params = vec![BuiltinTypes::INFERRED; param_count];
        let mut normal_inc = 0;
        let mut assoc_inc = 0;
        let success_len = rest
            .iter()
            .filter_map(|&child| {
                if child.kind != AstKind::FieldTy {
                    normal_inc += 1;
                    self.parse(child)
                        .ok()
                        .map(|ty| params.get_mut(normal_inc - 1).map(|p| *p = ty))
                        .flatten()
                } else {
                    assoc_inc += 1;
                    self.parse_filed_ty(base, child)
                        .ok()
                        .map(|(ty, index)| params[index + assoc_offset] = ty)
                }
            })
            .count();

        if success_len != rest.len() {
            return Err(());
        }

        let param_count = self.typec.param_count(base);
        if assoc_offset != success_len - assoc_inc {
            self.workspace.push(diag! {
                (span, self.current_file) => "wrong number of type parameters",
                (none) => "expected {}, got {}" { param_count, params.len() },
            });
            return Err(());
        }

        Ok(ty_factory!(self).instance(base, params.into_iter()))
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
        let mut reserved = self.typec.ty_lists.reserve(args.len());
        for &ast_arg in args {
            let [.., ty] = self.ast_data[ast_arg.children] else {
                unreachable!("{:?}", &self.ast_data[ast_arg.children]);
            };
            let Ok(ty) = ty_parser!(self, self.current_file).parse(ty) else {
                continue;
            };
            self.typec.ty_lists.push_to_reserved(&mut reserved, ty);
        }

        if !reserved.finished() {
            self.typec
                .ty_lists
                .fill_reserved(reserved, BuiltinTypes::ANY);
            return Err(());
        }

        Ok(self.typec.ty_lists.finish_reserved(reserved))
    }

    pub fn bounded_generics(
        &mut self,
        generics: AstEnt,
        init: &mut Vec<Ty>,
    ) -> errors::Result<Maybe<TyList>> {
        let old_len = init.len();
        self.bounded_generics_low(generics, init)?;
        let bumped = self.typec.ty_lists.bump_slice(&init);
        init.truncate(old_len);
        Ok(bumped)
    }

    pub fn bounded_generics_low(&mut self, generics: AstEnt, init: &mut Vec<Ty>) -> errors::Result {
        init.reserve(self.ast_data[generics.children].len());
        for &ast_param in &self.ast_data[generics.children] {
            let (&name, bounds) = self.ast_data[ast_param.children].split_first().unwrap();
            let name = self.interner.intern_str(span_str!(self, name.span));

            let bound = ty_parser!(self, self.current_file).parse_bound_sum(bounds)?;
            let param = ty_factory!(self).param_of(bound);
            let item = ScopeItem::new(name, param, ast_param.span, self.current_file, Vis::Priv);
            self.scope.push(item);

            // handle duplicate, all params need to be unique
            let param = init
                .iter()
                .rev()
                .find_map(|&p| (p == param).then(|| ty_factory!(self).next_param_of(param)))
                .unwrap_or(param);

            self.insert_bound_items(name, bound);

            init.push(param);
        }
        Ok(())
    }

    pub fn insert_bound_items(&mut self, name: Ident, bound: Ty) {
        let mut frontier = vec![bound];

        while let Some(bound) = frontier.pop() {
            let bound = self.typec.instance_base_of(bound);
            let TyKind::Bound { inherits, assoc_types, funcs } = self.typec.types[bound].kind else {
                unreachable!("{:?}", self.typec.types[bound].kind);
            };

            let mut push_to_scope = |(loc, ptr): (Loc, ScopePtr)| {
                let id = self.interner.intern(scoped_ident!(name, loc.name));
                let diag_loc = self.typec.loc_to_diag_loc(loc, self.interner).expand();
                let item = Item {
                    id,
                    ptr,
                    span: diag_loc.map(|loc| loc.span.expand()).flatten().into(),
                    module: diag_loc.map(|loc| loc.source).into(),
                    vis: Vis::Priv,
                };
                self.scope.push(item);
                println!("{}", &self.interner[id])
            };

            self.typec.ty_lists[assoc_types]
                .iter()
                .map(|&ty| (self.typec.types[ty].loc, ty.into()))
                .for_each(&mut push_to_scope);

            self.typec
                .bound_funcs
                .indexed(funcs)
                .map(|(id, &func)| (func.loc, id.into()))
                .for_each(&mut push_to_scope);

            frontier.extend(self.typec.ty_lists[inherits].iter().copied());
        }
    }

    pub fn generics(&mut self, generics: AstEnt) {
        let mut param = BuiltinTypes::TY_ANY;
        for &(mut ast_param) in &self.ast_data[generics.children] {
            if ast_param.kind != AstKind::Ident {
                ast_param = self.ast_data[ast_param.children][0];
            }
            let id = self.interner.intern_str(span_str!(self, ast_param.span));
            let item = ScopeItem::new(id, param, ast_param.span, self.current_file, Vis::Priv);
            self.scope.push(item);
            param = ty_factory!(self).next_param_of(param);
        }
    }

    pub fn assoc_types(
        &mut self,
        ast: AstEnt,
        bound_id: Ident,
        local_bound_id: Ident,
    ) -> Maybe<TyList> {
        let assoc_type_count = self.ast_data[ast.children]
            .iter()
            .filter(|item| matches!(item.kind, AstKind::BoundType { .. }))
            .count();

        let mut assoc_types = self.typec.ty_lists.reserve(assoc_type_count);

        for &item in self.ast_data[ast.children].iter() {
            let AstKind::BoundType { vis } = item.kind else {
                continue;
            };

            drop(self.assoc_type(item, bound_id, local_bound_id, &mut assoc_types, vis));
        }

        self.typec
            .ty_lists
            .fill_reserved(assoc_types, BuiltinTypes::INFERRED)
    }

    fn assoc_type(
        &mut self,
        ast: AstEnt,
        bound_id: Ident,
        local_bound_id: Ident,
        assoc_types: &mut Reserved<TyList>,
        vis: Vis,
    ) -> errors::Result {
        let &[generics, ast_name] = &self.ast_data[ast.children] else {
            unreachable!("{:?}", &self.ast_data[ast.children]);
        };

        let name = span_str!(self, ast_name.span);
        let id = self.interner.intern(scoped_ident!(bound_id, name));
        let local_id = self.interner.intern(scoped_ident!(local_bound_id, name));

        if let Some(prev) = self.typec.types.index(id) {
            let loc = self.typec.loc_of(prev, self.interner);
            self.duplicate_definition(ast.span, loc);
            return Err(());
        }

        let ty_ent = TyEnt {
            kind: TyKind::AssocType {
                index: self.typec.ty_lists.reserve_len(&assoc_types) as u32,
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8,
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
        };
        let ty = self.typec.types.insert_unique(id, ty_ent);
        self.typec.ty_lists.push_to_reserved(assoc_types, ty);

        let item = ModItem::new(local_id, ty, ast_name.span, vis);
        self.insert_scope_item(item);

        Ok(())
    }

    pub fn ident_chain_id(&mut self, ast: AstEnt) -> Ident {
        match ast.kind {
            AstKind::IdentChain => {
                let segments = self.ast_data[ast.children]
                    .iter()
                    .map(|child| self.packages.span_str(self.current_file, child.span))
                    .map(|str| (self.interner.intern_str(str), str))
                    .map(|(id, str)| self.scope.project(id, str))
                    .map(|segment| [InternedSegment::from(segment), "`".into()])
                    .flatten()
                    .take(
                        (self.ast_data[ast.children].len() * 2)
                            .checked_sub(1)
                            .unwrap_or(0),
                    ) // -1 for the excess
                    .collect::<Vec<_>>();

                self.interner.intern(&segments)
            }
            AstKind::Ident => {
                let str = self.packages.span_str(self.current_file, ast.span);
                let id = self.interner.intern_str(str);
                self.scope.project(id, str)
            }
            _ => unimplemented!(),
        }
    }

    insert_scope_item!();
    duplicate_definition!();
}
