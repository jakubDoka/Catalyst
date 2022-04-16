use cranelift_entity::{
    packed_option::ReservedValue,
    EntityList, PrimaryMap, SecondaryMap,
};

use crate::{error, error::Error, tir::Temp, *};
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
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> Result {
        let func_ent = &mut self.funcs[self.func];

        let ret = func_ent.sig.ret;
        let ast = self.func_ast[self.func];
        let header = self.ast.children(ast);
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


        if !self.body.ents[root].flags.contains(tir::Flags::TERMINATING) {
            self.expect_tir_ty(root, ret)?;
        }        

        self.scope.pop_frame();

        Ok(())
    }

    fn build_block(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;

        self.temp.mark_frame();
        self.scope.mark_frame();
        
        let mut final_expr = None;
        let mut terminating = false;
        for &stmt in self.ast.children(ast) {
            let expr = self.build_expr(stmt)?;
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

    fn build_expr(&mut self, ast: Ast) -> Result<Tir> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        let expr = match kind {
            ast::Kind::Binary => self.build_binary(ast)?,
            ast::Kind::Ident => self.build_ident(ast)?,
            ast::Kind::If => self.build_if(ast)?,
            ast::Kind::Return => self.build_return(ast)?,
            ast::Kind::Int(width) => self.build_int(ast, width)?,
            ast::Kind::Bool(value) => self.build_bool(ast, value)?,
            ast::Kind::Variable => self.build_variable(ast)?,
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

    pub fn build_break(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!();
        };

        let value = if value.is_reserved_value() {
            None
        } else {
            Some(self.build_expr(value)?)
        };
        let value_ty = value.map(|v| self.body.ents[v].ty).unwrap_or(self.nothing);

        let loop_expr = self
            .scope
            .get::<Tir>("<loop>", span)
            .map_err(Convert::convert)?;

        let loop_ty = &mut self.body.ents[loop_expr].ty;
        if loop_ty.is_reserved_value() {
            *loop_ty = value_ty;
        } else {
            if let Some(value) = value {
                let loop_ty = *loop_ty;
                self.expect_tir_ty(value, loop_ty)?;
            } else {
                return Err(Error::new(error::Kind::ExpectedValue, span));
            }
        }

        let result = {
            let kind = tir::Kind::Break(loop_expr, value.into());
            let ent = tir::Ent::with_flags(kind, self.nothing, tir::Flags::TERMINATING, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_loop(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[body_ast] = self.ast.children(ast) else {
            unreachable!();
        };

        let loop_slot = self.body.ents.push(Default::default());
        // break and continue will propagate side effects so we save the lookup
        
        self.scope
            .push_item("<loop>", scope::Item::new(loop_slot, span));
        
        let block = self.build_block(body_ast)?;

        self.scope.pop_item();

        let ent = &mut self.body.ents[loop_slot];
        ent.kind = tir::Kind::Loop(block);
        ent.span = span;
        if ent.ty.is_reserved_value() {
            // meas loop does not contain any break statements thus is infinite
            ent.flags.insert(tir::Flags::TERMINATING);
            ent.ty = self.nothing;
        }

        Ok(loop_slot)
    }

    fn build_call(&mut self, ast: Ast) -> Result<Tir> {
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
        let func = self.scope.get::<Func>(id, span).map_err(Convert::convert)?;

        let sig = self.funcs[func].sig;

        for &arg in &self.ast.children(ast)[1..] {
            let expr = self.build_expr(arg)?;
            args.push(expr);
        }

        /* check signature */
        {
            let arg_tys = self.types.cons.view(sig.args); // TODO: avoid allocation

            if arg_tys.len() != args.len() {
                let span = self.ast.nodes[ast].span;
                let kind = error::Kind::ArgCountMismatch(args.len(), arg_tys.len());
                return Err(Error::new(kind, span));
            }

            for (&arg, &ty) in args.iter().zip(arg_tys) {
                self.expect_tir_ty(arg, ty)?;
            }
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

    pub fn func_id(ty: ID, name: ID) -> ID {
        ty + name
    }

    fn build_dot_expr(&mut self, ast: Ast) -> Result<Tir> {
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

        let Some(&ty::Field { ty, .. }) = self.types.fields.get(id) else {
            return Err(Error::new(error::Kind::UnknownField, span));
        };

        let result = {
            let kind = tir::Kind::FieldAccess(header, id);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_bool(&mut self, ast: Ast, value: bool) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ty = self.bool;
        let kind = tir::Kind::BoolLit(value);
        let ent = tir::Ent::new(kind, ty, span);
        let result = self.body.ents.push(ent);
        Ok(result)
    }

    fn build_constructor(&mut self, ast: Ast) -> Result<Tir> {
        let &[name, body] = self.ast.children(ast) else {
            unreachable!("{:?}", self.ast.children(ast));
        };

        let span = self.ast.nodes[name].span;
        let ty = {
            let str = self.sources.display(span);
            self.scope.get::<Ty>(str, span).map_err(Convert::convert)?
        };

        self.build_constructor_low(ty, body)
    }

    fn build_constructor_low(&mut self, ty: Ty, body: Ast) -> Result<Tir> {
        let span = self.ast.nodes[body].span;
        let ty_id = self.types.ents[ty].id;

        let fields = {
            let ty::Kind::Struct(fields) = self.types.ents[ty].kind else {
                return Err(Error::new(error::Kind::ExpectedStruct, span));
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

            let index = {
                let Some(&ty::Field { index, .. }) = self.types.fields.get(id) else {
                    return Err(Error::new(error::Kind::UnknownField, span));
                };
                index as usize
            };

            let field_ty = fields[index];

            let value = match self.ast.nodes[expr].kind {
                ast::Kind::InlineConstructor => self.build_constructor_low(field_ty, expr)?,
                _ => self.build_expr(expr)?,
            };

            self.expect_tir_ty(value, field_ty)?;
            initial_values[index] = value;
        }

        if initial_values.iter().any(Tir::is_reserved_value) {
            return Err(Error::new(error::Kind::UninitializedFields, span));
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

    fn build_variable(&mut self, ast: Ast) -> Result<Tir> {
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
        self.body.ents[value].flags.insert(tir::Flags::ASSIGNABLE);

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

    fn build_int(&mut self, ast: Ast, width: i16) -> Result<Tir> {
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

            self.scope.get::<Ty>(name, span).unwrap()
        };

        let ent = tir::Ent::new(kind, ty, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_return(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!()
        };

        let value = if value.is_reserved_value() {
            self.expect_ty(self.nothing, self.funcs[self.func].sig.ret, span)?;
            None
        } else {
            let value = self.build_expr(value)?;
            self.expect_tir_ty(value, self.funcs[self.func].sig.ret)?;
            Some(value)
        };


        let result = {
            let kind = tir::Kind::Return(value.into());
            let ent = tir::Ent::with_flags(kind, self.nothing, tir::Flags::TERMINATING, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_if(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[cond, then, otherwise] = self.ast.children(ast) else {
            unreachable!()
        };

        let cond = self.build_expr(cond)?;
        self.expect_tir_ty(cond, self.bool)?;

        let then = self.build_block(then)?;
        let otherwise = if otherwise.is_reserved_value() {
            None
        } else {
            Some(self.build_block(otherwise)?)
        };

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

    fn build_ident(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let id = {
            let str = self.sources.display(span);
            ID::new(str)
        };


        let value = self.scope.get::<Tir>(id, span)
            .map_err(Convert::convert)?;

        Ok(value)
    }

    fn build_binary(&mut self, ast: Ast) -> Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[left, op, right] = self.ast.children(ast) else {
            unreachable!()
        };

        let left = self.build_expr(left)?;
        let right = self.build_expr(right)?;

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

        let func = self
            .scope
            .get::<Func>(id, op_span)
            .map_err(Convert::convert)?;

        /* sanity check */
        {
            let func_ent = &self.funcs[func];
            let args = self.types.cons.view(func_ent.sig.args);

            let arg_count = args.len();
            if arg_count != 2 {
                return Err(Error::new(
                    error::Kind::ArgCountMismatch(2, arg_count),
                    span,
                ));
            }

            let second_arg_ty = args[1];
            self.expect_tir_ty(right, second_arg_ty)?;
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

    fn build_assign(&mut self, left: Tir, right: Tir, span: Span) -> Result<Tir> {
        let tir::Ent {
            ty,
            flags,
            span: left_span,
            ..
        } = self.body.ents[left];
        
        if !flags.contains(tir::Flags::ASSIGNABLE) {
            return Err(Error::new(error::Kind::NotAssignable, left_span));
        }

        self.expect_tir_ty(right, ty)?;

        let result = {
            let kind = tir::Kind::Assign(left, right);
            let ent = tir::Ent::new(kind, ty, span);
            self.body.ents.push(ent)
        };


        Ok(result)
    }

    fn expect_tir_ty(&self, right: Tir, ty: Ty) -> Result {
        let tir::Ent {
            ty: expected, span, ..
        } = self.body.ents[right];
        self.expect_ty(ty, expected, span)
    }

    fn expect_ty(&self, ty: Ty, expected: Ty, span: Span) -> Result {
        if Some(ty) == expected.into() {
            Ok(())
        } else {
            Err(Error::new(error::Kind::TypeMismatch(ty, expected), span))
        }
    }

    pub fn parse_type(&mut self /* mut on purpose */, ty: Ast) -> Result<Ty> {
        parse_type(self.scope, self.ast, self.sources, ty)
    }

    pub fn binary_id(left: ID, op: ID) -> ID {
        ID::new("<binary>") + left + op
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