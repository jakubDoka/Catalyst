use std::ops::Not;

use crate::*;
use diags::*;
use packaging_t::span_str;
use parsing_t::*;
use scope::ScopeItem;
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
        let id = ident_chain_id!(self, ast);
        self.scope
            .get_concrete::<Ty>(id)
            .map_err(scope_error_handler!(self, ast.span, id, "type not found"))
    }

    fn parse_ptr_ty(&mut self, mutable: bool, ast: AstEnt) -> errors::Result<Ty> {
        let base = self.parse(self.ast_data[ast.children][0])?;
        Ok(ty_factory!(self).pointer_of(mutable, base))
    }

    fn parse_instance(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        let (&first, rest) = self.ast_data[ast.children].split_first().unwrap();

        let params = rest
            .iter()
            .filter_map(|&id| self.parse(id).ok())
            .collect::<Vec<_>>();
        if params.len() != rest.len() {
            return Err(());
        }

        let base = self.parse(first)?;

        let param_count = self.types.param_count(base);
        if params.len() != param_count {
            self.workspace.push(diag! {
                (ast.span, self.current_file) => "wrong number of type parameters",
                (none) => "expected {}, got {}" { param_count, params.len() },
            });
            return Err(());
        }

        Ok(ty_factory!(self).instance(base, &params))
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
        let mut reserved = self.types.slices.reserve(args.len());
        for &ast_arg in args {
            let [.., ty] = self.ast_data[ast_arg.children] else {
                unreachable!("{:?}", &self.ast_data[ast_arg.children]);
            };
            let ty = ty_parser!(self, self.current_file).parse(ty)?;
            self.types.slices.push_to_reserved(&mut reserved, ty);
        }
        Ok(self.types.slices.finish_reserved(reserved))
    }

    pub fn generics(&mut self, generics: AstEnt) -> errors::Result<Maybe<TyList>> {
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
        Ok(self.types.slices.bump_slice(&params))
    }
}
