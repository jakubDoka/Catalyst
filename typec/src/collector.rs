use cranelift_entity::{packed_option::ReservedValue, SecondaryMap};

use crate::*;
use lexer::*;
use modules::*;
use parser::*;

pub struct Collector<'a> {
    pub nothing: Ty,
    pub scope: &'a mut Scope,
    pub functions: &'a mut Funcs,
    pub types: &'a mut Types,
    pub modules: &'a mut Modules,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub type_ast: &'a mut SecondaryMap<Ty, Ast>,
    pub func_ast: &'a mut SecondaryMap<Func, Ast>,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub module: Module,
}

impl<'a> Collector<'a> {
    pub fn collect_items(&mut self) -> errors::Result {
        for (ast, &ast::Ent { kind, span, .. }) in self.ast.elements() {
            match kind {
                ast::Kind::Function => (),
                ast::Kind::Struct => self.collect_struct(ast)?,
                _ => (todo!("Unhandled top-level item:\n{}", self.sources.display(span))),
            }
        }

        for (ast, &ast::Ent { kind, span, .. }) in self.ast.elements() {
            match kind {
                ast::Kind::Function => self.collect_function(ast)?,
                ast::Kind::Struct => (),
                _ => todo!("Unhandled top-level item:\n{}", self.sources.display(span)),
            }
        }

        Ok(())
    }

    fn collect_struct(&mut self, ast: Ast) -> errors::Result {
        let source = self.ast.nodes[ast].span.source();
        let &[name, ..] = self.ast.children(ast) else {
            unreachable!();
        };

        let span = self.ast.nodes[name].span;
        let scope_id = {
            let str = self.sources.display(span);
            ID::new(str)
        };
        let id = self.modules[self.module].id + scope_id;
        let ent = ty::Ent {
            id,
            kind: ty::Kind::Unresolved,
            name: span,
        };
        let ty = self.types.ents.push(ent);
        self.type_ast[ty] = ast;

        let item = module::Item::new(scope_id, ty, span);
        drop(self.scope.insert(self.diagnostics, source, scope_id, item.to_scope_item()));
        self.modules[self.module].items.push(item);

        Ok(())
    }

    fn collect_function(&mut self, ast: Ast) -> errors::Result {
        let children = self.ast.children(ast);
        let current_span = self.ast.nodes[ast].span;
        let &[call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let sig = {
            let args = {
                let mut temp = vec![];
                for &ast in
                    &children[Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_ARG_END]
                {
                    let children = self.ast.children(ast);
                    let amount = children.len() - 1;
                    let ty = children[amount];
                    let ty = self.parse_type(ty)?;
                    for _ in 0..amount {
                        temp.push(ty);
                    }
                }

                self.types.cons.list(&temp)
            };

            let ret = if return_type.is_reserved_value() {
                self.nothing
            } else {
                self.parse_type(return_type)?
            };

            let call_conv = if call_conv.is_reserved_value() {
                Span::default()
            } else {
                self.ast.nodes[call_conv].span
            };

            Signature {
                call_conv,
                args,
                ret,
            }
        };

        let func = {
            let ent = func::Ent {
                sig,
                name: self.ast.nodes[name].span,
                ..Default::default()
            };
            let func = self.functions.push(ent);
            self.func_ast[func] = ast;
            func
        };

        let id = {
            let name = self.ast.nodes[name].span;
            self.sources.display(name).into()
        };

        {
            let module_item = module::Item::new(id, func, current_span);
            drop(self.scope.insert(self.diagnostics, current_span.source(), id, module_item.to_scope_item()));

            self.modules[self.module].items.push(module_item);
        }

        Ok(())
    }
}

impl TypeParser for Collector<'_> {
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &ast::Data, &mut errors::Diagnostics) {
        (self.scope, self.types, self.sources, self.ast, self.diagnostics)
    }
}