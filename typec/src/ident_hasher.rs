use ast::*;
use lexer::*;
use storage::*;
use typec_types::*;
use crate::IdentHasher;

impl<'a> IdentHasher<'a> {
    pub fn ident_id(&mut self, ast: Ast, owner: Option<(Ty, Span)>) -> errors::Result<ID> {
        self.ident_id_low(ast, owner).map(|(id, _)| id)
    }

    pub fn ident_id_low(
        &mut self,
        ast: Ast,
        owner: Option<(Ty, Span)>,
    ) -> errors::Result<(ID, Option<(Ty, Span)>)> {
        let children = self.ast_data.children(ast);
        match (children, owner) {
            (&[module, _, item], None) | (&[module, item], Some(_)) => {
                let module_id = {
                    let span = self.ast_data.nodes[module].span;
                    let id = self.sources.id_of(span);

                    match self.scope.get_concrete::<Source>(id) {
                        Ok(source) => ID::from(source),
                        Err(err) => todo!("{err:?}"),
                    }                    
                };

                let (ty, span) = if let Some(owner) = owner {
                    owner
                } else {
                    let span = self.ast_data.nodes[item].span;
                    let id = self.sources.id_of(span);
                    (
                        match self.scope.get_concrete::<Ty>(id) {
                            Ok(ty) => ty,
                            Err(err) => todo!("{err:?}"),
                        },
                        span
                    )
                };

                let id = {
                    let name = ast::id_of(item, self.ast_data, self.sources);
                    let ty = self.types.base_id_of(ty);
                    ID::owned_func(ty, name)
                };

                Ok((id + module_id, Some((ty, span))))
            }
            (&[module_or_type, item], None) => {
                let item_id = ast::id_of(item, self.ast_data, self.sources);

                let span = self.ast_data.nodes[module_or_type].span;
                let id = self.sources.id_of(span);

                let item = self.scope.get(id);
                let Ok(item) = item else {
                    todo!("{item:?}");
                };

                Ok(
                    if let Some(source) = item.pointer.may_read::<Source>() {
                        (item_id + ID::from(source), None)
                    } else if let Some(ty) = item.pointer.may_read::<Ty>() {
                        (ID::owned_func(self.types[ty].id, item_id), Some((ty, span)))
                    } else {
                        todo!("{item:?}");
                    }
                )
            }
            (&[], None) => return Ok((ast::id_of(ast, self.ast_data, self.sources), None)),
            (&[], Some((ty, span))) => {
                let name = ast::id_of(ast, self.ast_data, self.sources);
                let ty_id = self.types.base_id_of(ty);
                Ok((ID::owned_func(ty_id, name), Some((ty, span))))
            }
            _ => {
                self.diagnostics.push(TyError::InvalidPath {
                    loc: self.ast_data.nodes[ast].span,
                });
                Err(())
            }
        }
    }
}
