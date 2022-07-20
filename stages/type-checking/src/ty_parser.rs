use crate::*;
use diags::*;
use packaging_t::*;
use parsing_t::*;
use storage::*;
use type_checking_t::*;

impl TyParser<'_> {
    pub fn parse(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        match ast.kind {
            AstKind::Ident => self.parse_ident(ast),
            AstKind::PtrTy { mutable } => self.parse_ptr_ty(mutable, ast),
            AstKind::TyInstance => self.parse_instance(ast),
            _ => unimplemented!(),
        }
    }

    fn parse_ident(&mut self, ast: AstEnt) -> errors::Result<Ty> {
        let id = self.interner.intern_str(span_str!(self, ast.span));
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
}
