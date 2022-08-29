use std::{iter, ops::Not};

use crate::*;
use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl TyParser<'_> {
    pub fn parse_bound_sum(&mut self, ast: &[Ast]) -> errors::Result<VRef<Bound>> {
        let bounds = ast
            .iter()
            .filter_map(|&child| self.parse_bound(child).ok())
            .collect::<BumpVec<_>>();

        if bounds.len() != ast.len() {
            return Err(());
        }

        Ok(match bounds.len() {
            0 => Bound::ANY,
            1 => bounds[0],
            _ => bound_checker!(self).anon_bound_of(&bounds),
        })
    }

    pub fn parse_bound_to_ent(&mut self, ast: Ast) -> errors::Result<VRef<Bound>> {
        match ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.parse_bound_ident(ast),
            AstKind::TyInstance => self.parse_bound_instance(ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn parse_bound_ident(&mut self, ast: Ast) -> errors::Result<VRef<Bound>> {
        let ident = self.ident_chain_id(ast);
        self.get_from_scope_concrete::<Bound>(ident, ast.span, "bound not found", Reports::base)
    }

    fn parse_bound_instance(&mut self, ast: Ast) -> errors::Result<VRef<Bound>> {
        let [base, ref args @ ..] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let base = self.parse_bound_ident(base)?;

        let args = args
            .iter()
            .filter_map(|&child| self.parse(child).ok())
            .collect::<BumpVec<_>>();

        if args.len() != self.ast_data[ast.children].len() - 1 {
            return Err(());
        }

        Ok(bound_checker!(self).bound_instance_of(base, &args, &[]))
    }

    pub fn parse(&mut self, ast: Ast) -> errors::Result<VRef<Ty>> {
        match ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.parse_ident(ast),
            AstKind::PtrTy { mutable } => self.parse_ptr_ty(mutable, ast),
            AstKind::TyInstance => self.parse_instance(ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn parse_ident(&mut self, ast: Ast) -> errors::Result<VRef<Ty>> {
        let id = self.ident_chain_id(ast);
        self.get_from_scope_concrete(id, ast.span, "type", Reports::base)
    }

    scope_error_handler!(concrete);

    fn parse_ptr_ty(&mut self, mutable: bool, ast: Ast) -> errors::Result<VRef<Ty>> {
        let base = self.parse(self.ast_data[ast.children][0])?;
        Ok(ty_factory!(self).pointer_of(mutable, base))
    }

    fn parse_filed_ty(&mut self, base: VRef<Ty>, ast: Ast) -> errors::Result<(VRef<Ty>, usize)> {
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
        let index = self.typec.param_index(assoc_ty).ok_or_else(|| {
            self.workspace.push(diag! {
                (name.span, self.current_file) => "this is not an associated type"
            })
        })?;

        Ok((ty, index))
    }

    fn parse_instance(&mut self, ast: Ast) -> errors::Result<VRef<Ty>> {
        let (&first, rest) = self.ast_data[ast.children].split_first().unwrap();

        let base = self.parse(first)?;

        self.parse_instance_low(base, rest, ast.span)
    }

    pub fn parse_instance_low(
        &mut self,
        base: VRef<Ty>,
        rest: &[Ast],
        span: Span,
    ) -> errors::Result<VRef<Ty>> {
        let param_count = self.typec.param_count(base);
        let assoc_offset = param_count - self.typec.assoc_ty_count_of_bound(base);

        let mut params = iter::repeat(Ty::INFERRED)
            .take(assoc_offset)
            .chain(
                self.typec
                    .params_of_ty(base)
                    .iter()
                    .skip(assoc_offset)
                    .copied(),
            )
            .collect::<BumpVec<_>>();
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

        Ok(ty_factory!(self).instance(base, params.iter().cloned()))
    }

    pub fn sig(&mut self, cc: Ast, args: &[Ast], ret: Ast) -> errors::Result<Sig> {
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

    pub fn args(&mut self, args: &[Ast]) -> errors::Result<VSlice<VRef<Bound>>> {
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
            self.typec.ty_lists.fill_reserved(reserved, TyANY);
            return Err(());
        }

        Ok(self.typec.ty_lists.finish_reserved(reserved))
    }

    pub fn bounded_generics(&mut self, generics: Ast) -> errors::Result<VSlice<VRef<Bound>>> {
        let mut reserved = bumpvec![];
        self.bounded_generics_low(generics, &mut reserved)?;
        Ok(self.typec.ty_lists.bump(reserved))
    }

    pub fn bounded_generics_low(
        &mut self,
        generics: Ast,
        buffer: &mut BumpVec<VRef<Ty>>,
    ) -> errors::Result {
        buffer.reserve(self.ast_data[generics.children].len());
        for &ast_param in &self.ast_data[generics.children] {
            let (&name, bounds) = self.ast_data[ast_param.children].split_first().unwrap();
            let name = self.interner.intern_str(span_str!(self, name.span));

            let bound = ty_parser!(self, self.current_file).parse_bound_sum(bounds)?;
            let param = ty_factory!(self).param_of(bound);
            let item = ScopeItem::new(name, param, ast_param.span, self.current_file, Vis::Priv);
            self.scope.push(item);

            // handle duplicate, all params need to be unique
            let param = buffer
                .iter()
                .rev()
                .find_map(|&p| (p == param).then(|| ty_factory!(self).next_param_of(param)))
                .unwrap_or(param);

            self.insert_bound_items(name, bound);

            buffer.push(param);
        }

        Ok(())
    }

    pub fn insert_bound_items(&mut self, name: Ident, bound: VRef<Ty>) {
        let mut frontier = bumpvec![bound];

        while let Some(bound) = frontier.pop() {
            let bound = self.typec.instance_base(bound);
            let TyKind::SelfBound { inherits, assoc_type_count, funcs } = self.typec.types[bound].kind else {
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
            };

            self.typec
                .params_of_ty(bound)
                .iter()
                .rev()
                .take(assoc_type_count as usize)
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

    pub fn assoc_types(
        &mut self,
        ast: Ast,
        bound_id: Ident,
        local_bound_id: Ident,
        params: &mut BumpVec<VRef<Ty>>,
    ) -> usize {
        let prev_len = params.len();
        for &item in self.ast_data[ast.children].iter() {
            let AstKind::BoundType { vis } = item.kind else {
                continue;
            };

            let ty = self
                .assoc_type(item, bound_id, local_bound_id, vis, params.len())
                .unwrap_or(Ty::INFERRED);
            params.push(ty);
        }
        params.len() - prev_len
    }

    fn assoc_type(
        &mut self,
        ast: Ast,
        bound_id: Ident,
        local_bound_id: Ident,
        vis: Vis,
        index: usize,
    ) -> errors::Result<VRef<Ty>> {
        let &[generics, ast_name] = &self.ast_data[ast.children] else {
            unreachable!("{:?}", &self.ast_data[ast.children]);
        };

        let params = self.bounded_generics(generics)?;

        let name = span_str!(self, ast_name.span);
        let id = self.interner.intern(scoped_ident!(bound_id, name));
        let local_id = self.interner.intern(scoped_ident!(local_bound_id, name));

        if let Some(prev) = self.typec.types.index(id) {
            let loc = self.typec.loc_of(prev, self.interner);
            self.duplicate_definition(ast.span, loc);
            return Err(());
        }

        let ty_ent = Ty {
            kind: TyKind::Param {
                index: index as u32,
                bound: TyANY,
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            params,
            loc: Loc::new(
                ast_name.span,
                self.current_file,
                self.interner.intern_str(name),
            ),
        };
        let ty = self.typec.types.insert_unique(id, ty_ent);

        let item = ModItem::new(local_id, ty, ast_name.span, vis);
        self.insert_scope_item(item);

        Ok(ty)
    }

    pub fn ident_chain_id(&mut self, ast: Ast) -> Ident {
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
                    .collect::<BumpVec<_>>();

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

    pub fn push_generics(&mut self, generics: Ast, parsed_generics: VSlice<VRef<Bound>>) {
        for (&ast_param, param) in self.ast_data[generics.children]
            .iter()
            .zip(self.typec.ty_lists[parsed_generics].to_bumpvec())
        {
            let [name, ..] = self.ast_data[ast_param.children] else {
                unreachable!();
            };

            let id = self.interner.intern_str(span_str!(self, name.span));
            self.scope.push(ScopeItem::new(
                id,
                param,
                name.span,
                self.current_file,
                Vis::Priv,
            ));

            let TyKind::Param { bound, .. } = self.typec.types[param].kind else {
                unreachable!();
            };

            ty_parser!(self, self.current_file).insert_bound_items(id, bound);
        }
    }

    insert_scope_item!();
    duplicate_definition!();
}
