use cranelift_entity::{packed_option::ReservedValue, SecondaryMap};

use crate::{*, Error};
use lexer::*;
use modules::*;
use parser::*;

pub struct Collector<'a> {
    pub nothing: Ty,
    pub any: Ty,
    pub scope: &'a mut Scope,
    pub funcs: &'a mut Funcs,
    pub types: &'a mut Types,
    pub modules: &'a mut Modules,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub type_ast: &'a mut SecondaryMap<Ty, Ast>,
    pub func_ast: &'a mut SecondaryMap<Func, Ast>,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub ctx: &'a mut Context,
    pub module: Source,
}

impl<'a> Collector<'a> {
    pub fn collect_items<'f>(&mut self, elements: impl Iterator<Item = (Ast, &'f ast::Ent)> + Clone) -> errors::Result {
        for (ast, &ast::Ent { kind, span, .. }) in elements.clone() {
            if kind == ast::Kind::Tag {
                self.ctx.tags.push(ast);
                continue;
            }

            match kind {
                ast::Kind::Function(..) 
				| ast::Kind::Impl => (),
                ast::Kind::Struct => drop(self.collect_struct(ast)),
                ast::Kind::Bound => drop(self.collect_bound(ast)), // for now
                _ => (todo!("Unhandled top-level item:\n{}", self.sources.display(span))),
            }

            self.ctx.tags.clear();
        }

        for (ast, &ast::Ent { kind, span, .. }) in elements {
            if kind == ast::Kind::Tag {
                self.ctx.tags.push(ast);
                continue;
            }

            match kind {
                ast::Kind::Function(..) => drop(self.collect_function(None, ast)),
                ast::Kind::Impl => drop(self.collect_impl(ast)),
				ast::Kind::Struct
                | ast::Kind::Bound => (),
                _ => todo!("Unhandled top-level item:\n{}", self.sources.display(span)),
            }

            self.ctx.tags.clear();
        }
        
        // due to the unique nested structure of bounds, (being types and also functions),
        // we have to defer the scope insertion after all types have been inserted.
        {
            self.scope.mark_frame();

            let mut prev = None;
            while let Some(func) = self.ctx.bound_funcs.pop() {
                let ast = self.func_ast[func];
                let func::Kind::Owned(bound) = self.funcs[func].kind else {
                    unreachable!();
                };
                
                if Some(bound) != prev {
                    if prev.is_some() {
                        self.scope.pop_item();
                    }
                    let span = self.types.ents[bound].name;
                    self.scope.push_item("Self", scope::Item::new(bound, span));
                    prev = Some(bound);
                }
                
                drop(self.collect_function(Some(func), ast));
            }
    
            self.scope.pop_frame();
        }


        Ok(())
    }

	fn collect_impl(&mut self, ast: Ast) -> errors::Result {
		let &[ty, body] = self.ast.children(ast) else {
			unreachable!();
		};

		let span = self.ast.nodes[ty].span;
		let ty = self.parse_type(ty)?;

		self.scope.mark_frame();
		self.scope.push_item("Self", scope::Item::new(ty, span));

		for &func in self.ast.children(body) {
			let reserved = {
				let ent = func::Ent {
        			kind: func::Kind::Owned(ty),
					..Default::default()
				};
				self.funcs.push(ent)
			};
			self.collect_function(Some(reserved), func)?;
		}

		self.scope.pop_frame();

		Ok(())
	}

    fn collect_bound(&mut self, ast: Ast) -> errors::Result {
        let source = self.ast.nodes[ast].span.source();
        let &[name, body] = self.ast.children(ast) else {
            unreachable!();
        };

        let name = self.ast.nodes[name].span;
        let scope_id = {
            let str = self.sources.display(name);
            ID::new(str)
        };
        let id = self.modules[self.module].id + scope_id;

        let slot = self.types.ents.push(ty::Ent {
            id,
            name,
            kind: ty::Kind::Unresolved,
        });

        let funcs = {
            for &func in self.ast.children(body) {
                let func_id = self.funcs.push(func::Ent {
                    kind: func::Kind::Owned(slot),
                    ..Default::default()
                });
                self.func_ast[func_id] = func;
                self.ctx.bound_funcs.push(func_id);
                self.types.funcs.push_one(func_id);
            }
            self.types.funcs.close_frame()
        };

        self.types.ents[slot].kind = ty::Kind::Bound(funcs);
        self.type_ast[slot] = ast;

        {
            let item = module::Item::new(scope_id, slot, name);
            drop(self.scope.insert(self.diagnostics, source, scope_id, item.to_scope_item()));
            self.modules[self.module].items.push(item);
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

        {
            let item = module::Item::new(scope_id, ty, span);
            drop(self.scope.insert(self.diagnostics, source, scope_id, item.to_scope_item()));
            self.modules[self.module].items.push(item);
        }

        Ok(())
    }

    fn collect_function(&mut self, prepared: Option<Func>, ast: Ast) -> errors::Result<Func> {
        let children = self.ast.children(ast);
        let ast::Ent { span: current_span, kind: ast::Kind::Function(external), .. } = self.ast.nodes[ast] else {
            unreachable!();
        };
        let &[generics, call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let sig = {
			self.scope.mark_frame();

			let params = {
				if !generics.is_reserved_value() {
					for (i, &param) in self.ast.children(generics).iter().enumerate() {
						let name = self.ast.children(param)[0];
                        let span = self.ast.nodes[name].span;
						let str = self.sources.display(span);
						let ty = self.types.get_parameter(i, span, None);
						self.scope.push_item(str, scope::Item::new(ty, span));
						self.types.args.push_one(ty);
					}
				}

				self.types.args.close_frame()
			};

            let args = {
                for &ast in
                    &children[Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_ARG_END]
                {
                    let children = self.ast.children(ast);
                    let amount = children.len() - 1;
                    let ty = children[amount];
                    let ty = self.parse_type(ty)?;
                    for _ in 0..amount {
                        self.types.args.push_one(ty);
                    }
                }

                self.types.args.close_frame()
            };

			
            let ret = if return_type.is_reserved_value() {
                self.nothing
            } else {
				self.parse_type(return_type)?
            };
			
			self.scope.pop_frame();
            
			let call_conv = if call_conv.is_reserved_value() {
                Span::default()
            } else {
                self.ast.nodes[call_conv].span
            };


            Sig {
                call_conv,
				params,
                args,
                ret,
            }
        };

        let func = prepared.unwrap_or_else(|| self.funcs.push(Default::default()));
        
        let scope_id = {
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            if let func::Kind::Owned(owner) = self.funcs[func].kind {
                func::Builder::owned_func_id(self.types.ents[owner].id, ID::new(str))
            } else {
                ID::new(str)
            }
        };
        let id = self.modules[self.module].id + scope_id;

        {
            let is_entry = self.find_simple_tag("entry");
            let is_inline = self.find_simple_tag("inline");
            let flags = {
                (func::Flags::EXTERNAL & external) |
                (func::Flags::INLINE & is_inline.is_some()) |
                (func::Flags::ENTRY & is_entry.is_some()) |
                (func::Flags::GENERIC & !generics.is_reserved_value())
            };

            if flags.contains(func::Flags::ENTRY | func::Flags::GENERIC) {
                self.diagnostics.push(Error::GenericEntry {
                    tag: is_entry.unwrap(),
                    generics: self.ast.nodes[generics].span,
                    loc: self.ast.nodes[name].span, 
                })
            }

            let ent = func::Ent {
                id,
                sig,
                name: self.ast.nodes[name].span,
                flags,
                ..self.funcs[func]
            };
            self.funcs[func] = ent;
            self.func_ast[func] = ast;
        }

        {
            let module_item = module::Item::new(scope_id, func, current_span);
            drop(self.scope.insert(self.diagnostics, current_span.source(), scope_id, module_item.to_scope_item()));
            self.modules[self.module].items.push(module_item);
        }

        Ok(func)
    }

    pub fn find_simple_tag(&self, name: &str) -> Option<Span> {
        self.ctx.tags
            .iter()
            .rev()
            .map(|&tag| self.ast.nodes[tag].span)
            .find(|&span| self.sources.display(span)[1..].trim() == name)
    }
}

pub struct Context {
    pub bound_funcs: Vec<Func>,
    pub tags: Vec<Ast>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            bound_funcs: Vec::new(),
            tags: Vec::new(),
        }
    }
}

impl TypeParser for Collector<'_> {
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &mut Modules, &ast::Data, &mut errors::Diagnostics) {
        (self.scope, self.types, self.sources, self.modules, self.ast, self.diagnostics)
    }
}