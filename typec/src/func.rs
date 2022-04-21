use cranelift_entity::{
    packed_option::ReservedValue,
    EntityList, PrimaryMap, SecondaryMap,
};

use crate::{tir::Temp, Error, *};
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
    pub temp: &'a mut Temp,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let func_ent = &mut self.funcs[self.func];

        let ret = func_ent.sig.ret;
        let ast = self.func_ast[self.func];
        let header = self.ast.children(ast);
        let &[.., ret_ast, _] = header else {
            unreachable!();
        };
        let &body_ast = header.last().unwrap();

        if body_ast.is_reserved_value() {
            func_ent.kind = func::Kind::External;
            return Ok(());
        }

        self.scope.mark_frame();
        let args = {
            let ast_args =
                &header[Parser::FUNCTION_ARG_START..header.len() - Parser::FUNCTION_ARG_END];
            let args = self.types.cons.view(func_ent.sig.args);
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
            self.body.cons.list(&tir_args)
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
            self.temp.acc(expr);
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

            let slice = self.temp.frame_view();
            let items = self.body.cons.list(slice);

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
            ast::Kind::Variable(mutable) => self.build_variable(mutable, ast)?,
            ast::Kind::Constructor => self.build_constructor(ast)?,
            ast::Kind::DotExpr => self.build_dot_expr(ast)?,
            ast::Kind::Call => self.build_call(ast)?,
            ast::Kind::Block => self.build_block(ast)?,
            ast::Kind::Loop => self.build_loop(ast)?,
            ast::Kind::Break => self.build_break(ast)?,
            _ => todo!(
                "Unhandled expression ast {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        };
        Ok(expr)
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
                    self.types.ents[ty].id
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
            let args = self.body.cons.list(&[expr]);
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

        let mut args = vec![];
        let (id, span) = if self.ast.nodes[caller].kind == ast::Kind::DotExpr {
            let &[expr, name] = self.ast.children(caller) else {
                unreachable!();
            };

            let expr = self.build_expr(expr)?;
            args.push(expr);

            let span = self.ast.nodes[name].span;
            let name_id = {
                let str = self.sources.display(span);
                ID::new(str)
            };

            let ty_id = {
                let ty = self.body.ents[expr].ty;
                self.types.ents[ty].id
            };

            (Self::func_id(ty_id, name_id), span)
        } else {
            let span = self.ast.nodes[caller].span;
            let str = self.sources.display(span);
            (ID::new(str), span)
        };

        // TODO: Handle function pointer as field
        let func = self.scope.get::<Func>(self.diagnostics ,id, span)?;

        let sig = self.funcs[func].sig;

        // we first collect results, its in a way of recovery
        // TODO: This can be optimized when needed
        {
            let temp = self.ast.children(ast)[1..]
                .iter()
                .map(|&expr| self.build_expr(expr))
                .collect::<Vec<_>>();
            for i in temp {
                args.push(i?);
            }
        }
        

        /* check signature */
        {
            let arg_tys = self.types.cons.view(sig.args).to_vec(); // TODO: avoid allocation
            
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
                // fold prevents allocation
                .fold(Ok(()), |acc, err| acc.and(err))?;
        }

        let result = {
            let ty = sig.ret;
            let args = self.body.cons.list(&args);
            let kind = tir::Kind::Call(func, args);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_dot_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[header, field] = self.ast.children(ast) else {
            unreachable!();
        };

        let header = self.build_expr(header)?;
        let ty = self.body.ents[header].ty;

        let span = self.ast.nodes[field].span;
        let id = {
            let ty_id = self.types.ents[ty].id;
            let str = self.sources.display(span);
            ty::Builder::field_id(ty_id, ID::new(str))
        };

        let ty::Field { ty, .. } = self.find_field(ty, id, span)?;

        let result = {
            let kind = tir::Kind::FieldAccess(header, id);
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
            let str = self.sources.display(span);
            self.scope.get::<Ty>(self.diagnostics, str, span)?
        };

        self.build_constructor_low(ty, body)
    }

    fn build_constructor_low(&mut self, ty: Ty, body: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[body].span;
        let ty_id = self.types.ents[ty].id;

        let fields = {
            let ty::Kind::Struct(fields) = self.types.ents[ty].kind else {
                self.diagnostics.push(Error::ExpectedStruct {
                    got: ty,
                    loc: span,
                });
                return Err(());
            };

            self.types.cons.view(fields).to_vec() // TODO: don't allocate
        };

        let mut initial_values = vec![Tir::reserved_value(); fields.len()]; // TODO: don't allocate
        for &field in self.ast.children(body) {
            let &[name, expr] = self.ast.children(field) else {
                unreachable!();
            };

            let span = self.ast.nodes[name].span;
            let id = {
                let str = self.sources.display(span);
                ty::Builder::field_id(ty_id, ID::new(str))
            };

            let Ok(ty::Field { index, span: hint, .. }) = self.find_field(ty, id, span) else {
                continue;
            };
            let index = index as usize;

            let field_ty = fields[index];

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

            initial_values[index] = value;
        }

        if initial_values.iter().any(Tir::is_reserved_value) {
            let missing = initial_values
                .iter()
                .enumerate()
                .filter_map(|(index, &value)| (value == Tir::reserved_value()).then_some(index))
                .filter_map(|i| self.types.fields
                    .iter()
                    .find(|(_, field)| 
                        field.parent == ty && 
                        field.index == i as u32
                    )
                )
                .map(|(_, field)| field.span)
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
            let fields = self.body.cons.list(&initial_values);
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
                self.types.ents[ty].id
            };

            Self::binary_id(left_id, op_id)
        };

        let func = self.scope.get::<Func>(self.diagnostics, id, op_span)?;

        /* sanity check */
        {
            let func_ent = &self.funcs[func];
            let args = self.types.cons.view(func_ent.sig.args);

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
            let args = self.body.cons.list(&[left, right]);
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

    fn find_field(&mut self, on: Ty, id: ID, loc: Span) -> errors::Result<Field> {
        self.types.fields.get(id).cloned().ok_or_else(|| {
            let candidates = self.types.fields.iter()
                .filter_map(|(_, field)| (field.parent == on).then_some(field.span))
                .collect::<Vec<_>>();
            
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

    pub fn func_id(ty: ID, name: ID) -> ID {
        ty + name
    }
}

impl TypeParser for Builder<'_> {
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &ast::Data, &mut errors::Diagnostics) {
        (&mut self.scope, &mut self.types, &self.sources, &self.ast, &mut self.diagnostics)
    }
}

#[derive(Default)]
pub struct Ent {
    pub sig: Signature,
    pub name: Span,
    pub kind: Kind,
    pub body: Tir,
    pub args: EntityList<Tir>,
    pub id: ID,
}

impl Ent {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(PartialEq, Eq)]
pub enum Kind {
    Local,
    External,
    Builtin,
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Local
    }
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    //pub params: EntityList<Ty>,
    pub call_conv: Span,
    pub args: EntityList<Ty>,
    pub ret: Ty,
}

pub struct SignatureDisplay<'a> {
    pub sig: &'a Signature,
    pub sources: &'a Sources,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(sig: &'a Signature, sources: &'a Sources, types: &'a Types) -> Self {
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
        for (i, &ty) in self.types.cons.view(self.sig.args).iter().enumerate() {
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