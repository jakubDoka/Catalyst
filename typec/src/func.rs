use std::fmt::Write;

use cranelift_entity::{
    packed_option::ReservedValue,
    PrimaryMap, SecondaryMap,
};

use crate::{Error, *};
use lexer::*;
use parser::*;

pub type Funcs = PrimaryMap<Func, Ent>;

pub struct Builder<'a> {
    pub func: Func,
    pub nothing: Ty,
    pub bool: Ty,
    pub funcs: &'a mut Funcs,
    pub func_ast: &'a SecondaryMap<Func, Ast>,
    pub body: &'a mut tir::Data,
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub temp: &'a mut FramedStack<Tir>,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub modules: &'a mut Modules,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let func_ent = &mut self.funcs[self.func];

        let ret = func_ent.sig.ret;
        let ast = self.func_ast[self.func];
        let header = self.ast.children(ast);
        let &[generics, .., ret_ast, _] = header else {
            unreachable!();
        };
        let &body_ast = header.last().unwrap();

        if body_ast.is_reserved_value() {
            if func_ent.flags.contains(func::Flags::EXTERNAL) {
                func_ent.kind = func::Kind::External;
            } else {
                func_ent.kind = func::Kind::Ignored;
            }
            return Ok(());
        }

        self.scope.mark_frame();
        if let func::Kind::Owned(ty) = func_ent.kind {
            let span = self.ast.nodes[ast].span;
            self.scope.push_item("Self", scope::Item::new(ty, span));
        }

        if !generics.is_reserved_value() {
            for (i, &item) in self.ast.children(generics).iter().enumerate() {
                let name = self.ast.children(item)[0];
                let span = self.ast.nodes[name].span;
                let str = self.sources.display(span);
                let id = ID::new(str);
                
                for &bound in &self.ast.children(item)[1..] {
                    let Ok(ty) = self.parse_type(bound) else {
                        continue;
                    };
                    self.types.args.push_one(ty);
                }

                self.types.args.top_mut().sort_by_key(|ty| ty.0);
                let duplicates = self.types.args
                    .top()
                    .windows(2)
                    .any(|w| w[0] == w[1]);
                
                if duplicates {
                    self.diagnostics.push(Error::DuplicateBound {
                        loc: span,
                    });
                }

                let item = self.types.args
                    .top()
                    .iter()
                    .map(|&ty| self.types.ents[ty].id)
                    .fold(None, |acc, ty| acc.map(|id| id + ty).or(Some(ty)))
                    .map(|id| if let Some(item) = self.scope.weak_get::<Ty>(id) {
                        self.types.args.discard();
                        item   
                    } else {
                        let bounds = self.types.args.close_frame();
                        let ent = ty::Ent {
                            id,
                            name: span,
                            kind: ty::Kind::BoundCombo(bounds),
                        };
                        self.types.ents.push(ent)
                    });
                
                let ty = self.types.get_parameter(i, span, item);

                self.scope.push_item(id, scope::Item::new(ty, span));
            }
        }

        let args = {
            let ast_args =
                &header[Parser::FUNCTION_ARG_START..header.len() - Parser::FUNCTION_ARG_END];
            let args = self.types.args.get(self.funcs[self.func].sig.args);
            let mut tir_args = Vec::with_capacity(args.len());
            for (i, (&ast, &ty)) in ast_args.iter().zip(args).enumerate() {
                let &[name, ..] = self.ast.children(ast) else {
                    unreachable!();
                };
                let span = self.ast.nodes[name].span;

                let arg = {
                    let kind = tir::Kind::Argument(i as u32);
                    let ent = tir::Ent::new(kind, ty, span);
                    self.body.ents.push(ent)
                };

                {
                    let id = {
                        let str = self.sources.display(span);
                        ID::new(str)
                    };
                    let item = scope::Item::new(arg, span);
                    self.scope.push_item(id, item);
                }

                tir_args.push(arg);
            }
            self.body.cons.push(&tir_args)
        };

        let root = self.build_block(body_ast)?;
        self.funcs[self.func].body = root;
        self.funcs[self.func].args = args;


        if !self.body.ents[root].flags.contains(tir::Flags::TERMINATING) && ret != self.nothing {
            let because = Some(self.ast.nodes[ret_ast].span);
            self.expect_tir_ty(root, ret, |_, got, loc| Error::ReturnTypeMismatch {
                because,
                expected: ret,
                got,
                loc,
            })?;
        }        

        self.scope.pop_frame();

        Ok(())
    }

    fn build_block(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;

        self.temp.mark_frame();
        self.scope.mark_frame();
        
        let mut final_expr = None;
        let mut terminating = false;
        for &stmt in self.ast.children(ast) {
            let Ok(expr) = self.build_expr(stmt) else {
                continue; // Recover here
            };
            self.temp.push(expr);
            final_expr = Some(expr);
            terminating |= self.body.ents[expr].flags.contains(tir::Flags::TERMINATING);
            if terminating {
                break; // TODO: emit warning
            }
        }

        let result = {
            let ty = if let Some(expr) = final_expr {
                self.body.ents[expr].ty
            } else {
                self.nothing
            };

            let slice = self.temp.top_frame();
            let items = self.body.cons.push(slice);

            let kind = tir::Kind::Block(items);
            let flags = tir::Flags::TERMINATING & terminating;
            let ent = tir::Ent::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        self.temp.pop_frame();
        self.scope.pop_frame();

        Ok(result)
    }

    fn build_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        let expr = match kind {
            ast::Kind::Unary => self.build_unary(ast)?,
            ast::Kind::Binary => self.build_binary(ast)?,
            ast::Kind::Ident => self.build_ident(ast)?,
            ast::Kind::If => self.build_if(ast)?,
            ast::Kind::Return => self.build_return(ast)?,
            ast::Kind::Int(width) => self.build_int(ast, width)?,
            ast::Kind::Bool(value) => self.build_bool(ast, value)?,
            ast::Kind::Char => self.build_char(ast)?,
            ast::Kind::Variable(mutable) => self.build_variable(mutable, ast)?,
            ast::Kind::Constructor => self.build_constructor(ast)?,
            ast::Kind::DotExpr => self.build_dot_expr(ast)?,
            ast::Kind::Call => self.build_call(ast)?,
            ast::Kind::Block => self.build_block(ast)?,
            ast::Kind::Loop => self.build_loop(ast)?,
            ast::Kind::Break => self.build_break(ast)?,
            ast::Kind::Deref => self.build_deref(ast)?,
            _ => todo!(
                "Unhandled expression ast {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        };
        Ok(expr)
    }

    fn build_deref(&mut self, ast: Ast) -> errors::Result<Tir> {
        let expr = self.ast.children(ast)[0];
        let target = self.build_expr(expr)?;
        let ty = self.body.ents[target].ty;
        let deref_ty = self.types.base_of(ty);
        if ty == deref_ty {
            self.diagnostics.push(Error::NonPointerDereference {
                loc: self.ast.nodes[ast].span,
                ty,
            });
            return Err(());
        }
        Ok(self.deref_ptr(deref_ty, target))
    }

    fn build_char(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ent = tir::Ent::new(tir::Kind::CharLit, self.types.builtin.char, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_unary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[op, expr] = self.ast.children(ast) else {
            unreachable!()
        };

        let expr = self.build_expr(expr)?;

        let func = {
            let span = self.ast.nodes[op].span;
            let id = {
                let ty = {
                    let ty = self.body.ents[expr].ty;
                    self.types.id_of(ty)
                };

                let op = {
                    let str = self.sources.display(span);
                    ID::new(str)
                };

                Self::unary_id(ty, op)
            };

            self.scope.get::<Func>(self.diagnostics, id, span)?
        };

        let result = {
            let ty = self.funcs[func].sig.ret;
            let span = self.ast.nodes[ast].span;
            let args = self.body.cons.push(&[expr]);
            let kind = tir::Kind::Call(func, args);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    pub fn unary_id(ty: ID, op: ID) -> ID {
        ID::new("<unary>") + ty + op
    }

    fn build_break(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!();
        };

        let value = if value.is_reserved_value() {
            None
        } else {
            Some(self.build_expr(value)?)
        };

        let loop_expr = self
            .scope
            .get::<Tir>(self.diagnostics, "<loop>", span)?;

        {
            let tir::Kind::LoopInProgress(ret, infinite) = &mut self.body.ents[loop_expr].kind else {
                unreachable!();
            };

            *infinite = false;

            if let Some(ret) = ret.expand() {
                let tir::Ent { span: ret_span, ty, .. } = self.body.ents[ret];
                if let Some(value) = value {
                    self.expect_tir_ty(value, ty, |_, got, loc| Error::BreakValueTypeMismatch {
                        because: ret_span,
                        expected: ty,
                        got,
                        loc,
                    })?;
                } else {    
                    self.diagnostics.push(Error::MissingBreakValue {
                        because: ret_span,
                        expected: ty,
                        loc: span,
                    });
                    return Err(());
                }
            } else {
                *ret = value.into();
            }
        }


        let result = {
            let kind = tir::Kind::Break(loop_expr, value.into());
            let ent = tir::Ent::with_flags(kind, self.nothing, tir::Flags::TERMINATING, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_loop(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[body_ast] = self.ast.children(ast) else {
            unreachable!();
        };

        let loop_slot = {
            let kind = tir::Kind::LoopInProgress(None.into(), true);
            let ent = tir::Ent::new(kind, self.nothing, span);
            self.body.ents.push(ent)
        };

        // break and continue will propagate side effects so we save the lookup
        
        self.scope
            .push_item("<loop>", scope::Item::new(loop_slot, span));
        
        let block = self.build_block(body_ast)?;

        self.scope.pop_item();

        
        {
            let tir::Kind::LoopInProgress(ret, infinite) = self.body.ents[loop_slot].kind else {
                unreachable!();
            };
            let ty = ret.map(|ret| self.body.ents[ret].ty).unwrap_or(self.nothing);
            let flags = tir::Flags::TERMINATING & infinite;
            let kind = tir::Kind::Loop(block);
            let span = span;
            self.body.ents[loop_slot] = tir::Ent::with_flags(kind, ty, flags, span);
        }
        
        Ok(loop_slot)
    }

    fn build_call(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[caller, ..] = self.ast.children(ast) else {
            unreachable!();
        };

        let (id, span, mut obj) = if self.ast.nodes[caller].kind == ast::Kind::DotExpr {
            let &[expr, name] = self.ast.children(caller) else {
                unreachable!();
            };

            let expr = self.build_expr(expr)?;
            let ty = {
                let ty = self.body.ents[expr].ty;
                self.types.base_of(ty)
            };

            let span = self.ast.nodes[name].span;
            let name_id = self.ident_id(name, Some(ty))?;

            (name_id, span, Some(expr))
        } else {
            let span = self.ast.nodes[caller].span;
            let name_id = self.ident_id(caller, None)?;
            (name_id, span, None)
        };

        // TODO: Handle function pointer as field
        let func = self.scope.get::<Func>(self.diagnostics ,id, span)?;
        
        
        let Ent { sig, flags, .. } = self.funcs[func];
        
        let arg_tys = self.types.args.get(sig.args).to_vec(); // TODO: avoid allocation
        
        // handle auto ref or deref
        if let (Some(obj), Some(&expected)) = (obj.as_mut(), arg_tys.first()) {
            let ty = self.body.ents[*obj].ty;
            if ty != expected { 
                if self.types.base_of(expected) == ty {
                    *obj = self.take_ptr(expected, *obj);
                } else if self.types.base_of(ty) == expected {
                    *obj = self.deref_ptr(expected, *obj);
                }
            }
        }
        
        // we first collect results, its in a way of recovery
        // TODO: This can be optimized when needed
        let args = {
            let mut vec = Vec::with_capacity(arg_tys.len() + obj.is_some() as usize);
            obj.map(|obj| vec.push(obj));
            // error recovery
            self.ast
                .children(ast)[1..]
                .iter()
                .fold(Ok(()), |acc, &arg| 
                    self.build_expr(arg)
                        .map(|arg| vec.push(arg))
                        .and(acc)
                )?;
            vec
        };
        

        let (ret, func) = {
            let because = self.funcs[func].name;
            if arg_tys.len() != args.len() {
                let loc = self.ast.nodes[ast].span;
                self.diagnostics.push(Error::FunctionParamMismatch {
                    because,
                    expected: arg_tys.len(),
                    got: args.len(),
                    loc,
                });
                return Err(());
            }

            if !flags.contains(Flags::GENERIC) {
                // here we type check all arguments and then check for errors
                args
                    .iter()
                    .zip(arg_tys)
                    .map(|(&arg, ty)| {
                        self.expect_tir_ty(arg, ty, |_, got, loc| Error::CallArgTypeMismatch {
                            because,
                            expected: ty,
                            got,
                            loc,
                        })
                    })
                    .fold(Ok(()), |acc, err| acc.and(err))?;
                
                (sig.ret, func)
            } else {
                let params = self.types.args.get(sig.params);
                let mut param_slots = vec![Ty::default(); params.len()];

                args
                    .iter()
                    .zip(arg_tys)
                    .map(|(&arg, ty)| {
                        if let ty::Kind::Param(index, _bound) = self.types.ents[ty].kind {
                            let slot = param_slots[index as usize];
                            if slot.is_reserved_value() {
                                param_slots[index as usize] = self.body.ents[arg].ty;
                                return Ok(());
                            }
                        }
                        self.expect_tir_ty(arg, ty, |_, got, loc| Error::CallArgTypeMismatch {
                            because,
                            expected: ty,
                            got,
                            loc,
                        })    
                    })
                    // fold prevents allocation
                    .fold(Ok(()), |acc, err| acc.and(err))?;
                
                if param_slots.contains(&Ty::default()) {
                    todo!()
                }

                let ret = if let ty::Kind::Param(index, _bound) = self.types.ents[sig.ret].kind {
                    param_slots[index as usize]
                } else {
                    sig.ret
                };

                let func = {
                    let i_params = self.types.args.push(&param_slots);
                    let sig = Sig {
                        params: i_params,
                        ..sig
                    };
                    let ent = Ent {
                        sig,
                        kind: Kind::Instance(func),
                        ..self.funcs[func]
                    };
                    self.funcs.push(ent)
                };

                (ret, func)
            }
        };

        let result = {
            let args = self.body.cons.push(&args);
            let kind = tir::Kind::Call(func, args);
            let ent = tir::Ent::new(kind, ret, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn take_ptr(&mut self, ptr_ty: Ty, target: Tir) -> Tir {
        let ent = &mut self.body.ents[target];
        ent.flags.insert(tir::Flags::SPILLED);
        let span = ent.span;
        let kind = tir::Kind::TakePointer(target);
        let ent = tir::Ent::new(kind, ptr_ty, span);
        self.body.ents.push(ent)
    }

    fn deref_ptr(&mut self, deref_ty: Ty, target: Tir) -> Tir {
        let span = self.body.ents[target].span;
        let kind = tir::Kind::DerefPointer(target);
        let deref = tir::Ent::new(kind, deref_ty, span);    
        self.body.ents.push(deref)
    }

    fn ident_id(&mut self, ast: Ast, owner: Option<Ty>) -> errors::Result<ID> {
        let children = self.ast.children(ast);
        match (children, owner) {
            (&[module, _, item], None) | (&[module, item], Some(_)) => {
                let module_id = {
                    let span = self.ast.nodes[module].span;
                    let id = {
                        let str = self.sources.display(span);
                        ID::new(str)
                    };

                    let source = self.scope.get::<Source>(self.diagnostics, id, span)?;
                    ID::from(source)
                };
                
                let id = {
                    let name = {
                        let span = self.ast.nodes[item].span;
                        let str = self.sources.display(span);
                        ID::new(str)
                    };
                    
                    let ty = { 
                        let ty = if let Some(owner) = owner {
                            owner
                        } else {
                            let span = self.ast.nodes[item].span;
                            let id = {
                                let str = self.sources.display(span);
                                ID::new(str)
                            };
                            self.scope.get::<Ty>(self.diagnostics, id, span)?
                        };

                        self.types.id_of(ty)
                    };

                    Self::owned_func_id(ty, name)
                };

                Ok(id + module_id)
            }
            (&[module_or_type, item], None) => {
                let item_id = {
                    let span = self.ast.nodes[item].span;
                    let str = self.sources.display(span);
                    ID::new(str)
                };

                
                let span = self.ast.nodes[module_or_type].span;
                let id = {
                    let str = self.sources.display(span);
                    ID::new(str)
                };

                Ok(if let Some(source) = self.scope.may_get::<Source>(self.diagnostics, id, span)? {
                    item_id + ID::from(source)
                } else {
                    let ty = self.scope.get::<Ty>(self.diagnostics, id, span)?;
                    Self::owned_func_id(self.types.id_of(ty), item_id)
                })
            }
            (&[], None) => {
                let span = self.ast.nodes[ast].span;
                let str = self.sources.display(span);
                return Ok(ID::new(str));
            }
            (&[], Some(ty)) => {
                let name = {
                    let span = self.ast.nodes[ast].span;
                    let str = self.sources.display(span);
                    ID::new(str)
                };

                let ty = self.types.id_of(ty);
                Ok(Self::owned_func_id(ty, name))
            }
            _ => {
                self.diagnostics.push(Error::InvalidPath {
                    loc: self.ast.nodes[ast].span,
                });
                Err(())
            },
        }

    }

    fn build_dot_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[header, field] = self.ast.children(ast) else {
            unreachable!();
        };

        let mut header = self.build_expr(header)?;
        
        let ty = self.body.ents[header].ty;
        let base_ty = self.types.base_of(ty);
        
        let span = self.ast.nodes[field].span;
        let field_id = {
            let id = {
                let ty_id = self.types.ents[base_ty].id;
                let str = self.sources.display(span);
                ty::Builder::field_id(ty_id, ID::new(str))
            };
            self.find_field(ty, id, span)?
        };

        if base_ty != ty {
            header = self.deref_ptr(base_ty, header);
        }

        let result = {
            let ty = self.types.sfields[field_id].ty;
            let kind = tir::Kind::FieldAccess(header, field_id);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_bool(&mut self, ast: Ast, value: bool) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ty = self.bool;
        let kind = tir::Kind::BoolLit(value);
        let ent = tir::Ent::new(kind, ty, span);
        let result = self.body.ents.push(ent);
        Ok(result)
    }

    fn build_constructor(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[name, body] = self.ast.children(ast) else {
            unreachable!("{:?}", self.ast.children(ast));
        };

        let span = self.ast.nodes[name].span;
        let ty = {
            let id = self.ident_id(name, None)?;
            self.scope.get::<Ty>(self.diagnostics, id, span)?
        };

        self.build_constructor_low(ty, body)
    }

    fn build_constructor_low(&mut self, ty: Ty, body: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[body].span;
        let ty_id = self.types.ents[ty].id;

        let ty::Kind::Struct(fields) = self.types.ents[ty].kind else {
            self.diagnostics.push(Error::ExpectedStruct {
                got: ty,
                loc: span,
            });
            return Err(());
        };           

        let mut initial_values = vec![Tir::reserved_value(); self.types.sfields.get(fields).len()]; // TODO: don't allocate
        for &field in self.ast.children(body) {
            let &[name, expr] = self.ast.children(field) else {
                unreachable!();
            };

            let span = self.ast.nodes[name].span;
            let id = {
                let str = self.sources.display(span);
                ty::Builder::field_id(ty_id, ID::new(str))
            };

            let Ok(field) = self.find_field(ty, id, span) else {
                continue;
            };

            let ty::SFieldEnt { ty: field_ty, span: hint, index, .. } = self.types.sfields[field];

            let Ok(value) = (match self.ast.nodes[expr].kind {
                ast::Kind::InlineConstructor => self.build_constructor_low(field_ty, expr),
                _ => self.build_expr(expr),
            }) else {
                continue;
            };

            // we ignore this to report more errors
            drop(self.expect_tir_ty(value, field_ty, |_, got, loc| Error::ConstructorFieldTypeMismatch {
                because: hint,
                expected: field_ty,
                got,
                loc,
            }));

            initial_values[index as usize] = value;
        }

        if initial_values.iter().any(Tir::is_reserved_value) {
            let missing = initial_values
                .iter()
                .enumerate()
                .filter_map(|(index, &value)| (value == Tir::reserved_value()).then_some(index))
                .map(|i| self.types.sfields.get(fields)[i].span)
                .collect::<Vec<_>>();

            self.diagnostics.push(Error::ConstructorMissingFields {
                on: ty,
                missing,
                loc: span,
            });

            return Err(());
        }

        let result = {
            let span = self.ast.nodes[body].span;
            let fields = self.body.cons.push(&initial_values);
            let kind = tir::Kind::Constructor(fields);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_variable(&mut self, mutable: bool, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[name, value] = self.ast.children(ast) else {
            unreachable!();
        };

        let id = {
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            ID::new(str)
        };

        let value = self.build_expr(value)?;
        self.body.ents[value].flags.insert(tir::Flags::ASSIGNABLE & mutable);

        {
            let item = scope::Item::new(value, span);
            self.scope.push_item(id, item);
        }

        let result = {
            let kind = tir::Kind::Variable(value);
            let ent = tir::Ent::new(kind, self.nothing, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_int(&mut self, ast: Ast, width: i16) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let kind = tir::Kind::IntLit(width);
        let ty = {
            let name = match width {
                8 => "i8",
                16 => "i16",
                32 => "i32",
                64 => "i64",
                _ => "int",
            };

            self.scope.get::<Ty>(self.diagnostics, name, span).unwrap()
        };

        let ent = tir::Ent::new(kind, ty, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_return(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!()
        };

        let ret = self.funcs[self.func].sig.ret;

        let value = if value.is_reserved_value() {
            self.expect_ty(self.nothing, ret, |s| Error::UnexpectedReturnValue {
                because: s.funcs[s.func].name,
                loc: span,
            })?;
            None
        } else {
            let value = self.build_expr(value)?;
            self.expect_tir_ty(value, ret, |s, got, loc| {
                let &[.., ret_ast, _] = s.ast.children(s.func_ast[s.func]) else {
                    unreachable!();
                };
                let ret_span = (!ret_ast.is_reserved_value()).then(|| s.ast.nodes[ret_ast].span);
                Error::ReturnTypeMismatch {
                    because: ret_span,
                    expected: ret,
                    got,
                    loc,
                }
            })?;
            Some(value)
        };


        let result = {
            let kind = tir::Kind::Return(value.into());
            let ent = tir::Ent::with_flags(kind, self.nothing, tir::Flags::TERMINATING, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_if(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[cond, then, otherwise] = self.ast.children(ast) else {
            unreachable!()
        };

        let cond = self.build_expr(cond)?;
        drop(self.expect_tir_ty(cond, self.bool, |_, got, loc| Error::IfConditionTypeMismatch {
            got, 
            loc,
        }));

        let then = self.build_block(then);
        let otherwise = if otherwise.is_reserved_value() {
            None
        } else {
            Some(self.build_block(otherwise)?)
        };
        let then = then?;

        let ty = if let Some(otherwise) = otherwise {
            let then = self.body.ents[then].ty;
            let otherwise = self.body.ents[otherwise].ty;
            if then == otherwise {
                then
            } else {
                self.nothing
            }
        } else {
            self.nothing
        };

        let flags = {
            let ents = &self.body.ents;
            
            let then_flags = ents[then].flags;
            let otherwise_flags = otherwise.map(|otherwise| ents[otherwise].flags)
                .unwrap_or(tir::Flags::empty());
            
            (then_flags & otherwise_flags) & 
                tir::Flags::TERMINATING
        };
        
        let result = {
            let kind = tir::Kind::If(cond, then, otherwise.into());
            let ent = tir::Ent::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_ident(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let id = {
            let str = self.sources.display(span);
            ID::new(str)
        };


        let value = self.scope.get::<Tir>(self.diagnostics, id, span)?;

        // this allows better error messages
        let result = {
            let mut copy = self.body.ents[value];
            copy.kind = tir::Kind::Access(value);
            copy.span = span;        
            self.body.ents.push(copy)
        };

        Ok(result)
    }

    fn build_binary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[left, op, right] = self.ast.children(ast) else {
            unreachable!()
        };

        // we walk the ast even if something fails for better error diagnostics
        let (left, right) = {
            let left = self.build_expr(left);
            let right = self.build_expr(right);
            (left?, right?)
        };

        let op_span = self.ast.nodes[op].span;
        let id = {
            let op_id = {
                let str = self.sources.display(op_span);
                if str == "=" {
                    return self.build_assign(left, right, span);
                }

                ID::new(str)
            };

            let left_id = {
                let ty = self.body.ents[left].ty;
                self.types.id_of(ty)
            };

            Self::binary_id(left_id, op_id)
        };

        let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, op_span) else {
            self.diagnostics.push(Error::BinaryOperatorNotFound {
                left_ty: self.body.ents[left].ty,
                right_ty: self.body.ents[right].ty,
                loc: op_span,
            });
            return Err(());
        };

        /* sanity check */
        {
            let func_ent = &self.funcs[func];
            let args = self.types.args.get(func_ent.sig.args);

            let arg_count = args.len();
            if arg_count != 2 {
                self.diagnostics.push(Error::OperatorArgCountMismatch {
                    because: func_ent.name,
                    expected: 2,
                    got: arg_count,
                    loc: op_span,
                });
                return Err(());
            }

            let expected = args[1];
            self.expect_tir_ty(right, expected, |_, got, loc| Error::BinaryTypeMismatch {
                expected,
                got,
                loc,
            })?;
        }

        let tir = {
            let ty = self.funcs[func].sig.ret;
            let args = self.body.cons.push(&[left, right]);
            let kind = tir::Kind::Call(func, args);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(tir)
    }

    fn build_assign(&mut self, left: Tir, right: Tir, span: Span) -> errors::Result<Tir> {
        let tir::Ent {
            ty,
            flags,
            span: left_span,
            kind,
        } = self.body.ents[left];
        
        if !flags.contains(tir::Flags::ASSIGNABLE) {
            let because = if let tir::Kind::Access(here) = kind {
                Some(self.body.ents[here].span)
            } else {
                None
            };
            self.diagnostics.push(Error::AssignToNonAssignable {
                because,
                loc: left_span,
            });
        }

        self.expect_tir_ty(right, ty, |_, got, loc| Error::AssignTypeMismatch {
            because: left_span,
            expected: ty,
            got,
            loc,
        })?;

        let result = {
            let kind = tir::Kind::Assign(left, right);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };


        Ok(result)
    }

    fn find_field(&mut self, on: Ty, id: ID, loc: Span) -> errors::Result<SField> {
        self.types.sfield_lookup.get(id).map(|f| f.field).ok_or_else(|| {
            let candidates = if let ty::Kind::Struct(fields) = self.types.ents[on].kind {
                self.types.sfields.get(fields).iter().map(|sfref| sfref.span).collect()
            } else {
                Vec::new()
            };
            
            self.diagnostics.push(Error::UnknownField {
                candidates,
                on,
                loc,
            });
        })
    }

    fn expect_tir_ty(&mut self, right: Tir, ty: Ty, err_fact: impl Fn(&mut Self, Ty, Span) -> Error) -> errors::Result {
        let tir::Ent {
            ty: got, span, ..
        } = self.body.ents[right];
        self.expect_ty(ty, got, |s| err_fact(s, got, span))
    }

    fn expect_ty(&mut self, ty: Ty, got: Ty, err_fact: impl Fn(&mut Self) -> Error) -> errors::Result {
        if ty != got {
             
            let error = err_fact(self);
            self.diagnostics.push(error);
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn binary_id(left: ID, op: ID) -> ID {
        ID::new("<binary>") + left + op
    }

    pub fn owned_func_id(ty: ID, name: ID) -> ID {
        ty + name
    }
}

impl TypeParser for Builder<'_> {
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &mut Modules, &ast::Data, &mut errors::Diagnostics) {
        (self.scope, self.types, &self.sources, self.modules, &self.ast, &mut self.diagnostics)
    }
}

#[derive(Copy, Clone, Default)]
pub struct Ent {
    pub sig: Sig,
    pub name: Span,
    pub kind: Kind,
    pub body: Tir,
    pub args: TirList,
    pub id: ID,
    pub flags: Flags,
}

impl Ent {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_link_name(&self, types: &Types, sources: &Sources, buffer: &mut String) {
        buffer.write_str(sources.display(self.name)).unwrap();
        if !self.sig.params.is_reserved_value() {
            buffer.write_char('[').unwrap();
            for &ty in types.args.get(self.sig.params) {
                ty.display(types, sources, buffer).unwrap();
                buffer.write_char(',').unwrap();
            }
            buffer.pop().unwrap();
            buffer.write_char(']').unwrap();
        }
    }
}

bitflags::bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        const ENTRY = 1 << 0;
        const GENERIC = 1 << 1;
        const INLINE = 1 << 2;
        const EXTERNAL = 1 << 3;
    }
}

crate::impl_bool_bit_and!(Flags);

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    Local,
    External,
    Owned(Ty),
    Instance(Func),
    Builtin,
    Ignored,
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Local
    }
}

#[derive(Clone, Copy, Default)]
pub struct Sig {
    pub params: TyList,
    pub call_conv: Span,
    pub args: TyList,
    pub ret: Ty,
}

pub struct SignatureDisplay<'a> {
    pub sig: &'a Sig,
    pub sources: &'a Sources,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(sig: &'a Sig, sources: &'a Sources, types: &'a Types) -> Self {
        SignatureDisplay {
            sig,
            types,
            sources,
        }
    }
}

impl std::fmt::Display for SignatureDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, &ty) in self.types.args.get(self.sig.args).iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty::Display::new(self.types, self.sources, ty))?;
        }
        write!(f, ") -> {}", ty::Display::new(self.types, self.sources, self.sig.ret))?;
        Ok(())
    }
}

lexer::gen_entity!(Func);
lexer::gen_entity!(FuncList);