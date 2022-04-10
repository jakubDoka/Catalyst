use cranelift_entity::packed_option::ReservedValue;
use lexer::{Sources, SourcesExt, Span, ID, ListPoolExt, SpanDisplay};
use modules::{scope::{self, Scope}, module::{Module, self}, logic::Modules};
use parser::{
    ast::{self, Ast},
    {Convert, Parser},
};

use crate::{
    error::{Error, self},
    func::{self, Func, Funcs, Signature},
    tir::{
        block::Block,
        inst,
        value::{self, Value},
    },
    ty::{Ty, Types}, builder::Builder,
};

type Result<T = ()> = std::result::Result<T, Error>;

pub struct Collector<'a> {
    pub scope: &'a mut Scope,
    pub functions: &'a mut Funcs,
    pub types: &'a mut Types,
    pub modules: &'a mut Modules,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
}

impl<'a> Collector<'a> {
    pub fn collect_items(&mut self, module: Module) -> Result {
        for (ast, &ast::Ent { kind, span, .. }) in self.ast.elements() {
            match kind {
                ast::Kind::Function => self.collect_function(ast, module)?,
                _ => todo!("Unhandled top-level item:\n{}", self.sources.display(span)),
            }
        }

        Ok(())
    }

    fn collect_function(&mut self, ast: Ast, module: Module) -> Result {
        let children = self.ast.children(ast);
        let current_span = self.ast.nodes[ast].span;
        let &[call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let mut args = vec![];
        for &ast in &children[Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_ARG_END]
        {
            let children = self.ast.children(ast);
            let amount = children.len() - 1;
            let ty = children[amount];
            let ty = self.parse_type(ty)?;
            for _ in 0..amount {
                args.push(ty);
            }
        }

        let args = self.types.cons.list(&args);

        let ret = if return_type.is_reserved_value() {
            None
        } else {
            Some(self.parse_type(return_type)?)
        }
        .into();

        let call_conv = if call_conv.is_reserved_value() {
            Span::default()
        } else {
            self.ast.nodes[call_conv].span
        };
        let sig = Signature {
            call_conv,
            args,
            ret,
        };
        let name = self.ast.nodes[name].span;
        let ent = func::Ent {
            sig,
            ast,
            name, 
            ..Default::default()
        };
        let func = self.functions.ents.push(ent);
        let id = self.sources.display(name).into();
        self.scope
            .insert(
                current_span.source(),
                id,
                scope::Item::new(func, current_span),
            )
            .map_err(Convert::convert)?;
        
        let module_item = module::Item::new(id, func, current_span);
        self.modules[module].items.push(module_item);

        Ok(())
    }

    pub fn parse_type(&mut self /* mut on purpose */, ty: Ast) -> Result<Ty> {
        parse_type(self.scope, self.ast, self.sources, ty)
    }
}

pub fn parse_type(scope: &Scope, ast: &ast::Data, sources: &Sources, ty: Ast) -> Result<Ty> {
    let ast::Ent { kind, span, .. } = ast.nodes[ty];
    match kind {
        ast::Kind::Ident => {
            return scope
                .get(sources.display(span), span)
                .map_err(Convert::convert);
        }
        _ => todo!(
            "Unhandled type expr {:?}: {}",
            kind,
            sources.display(span)
        ),
    }
}

pub struct FuncBuilder<'a> {
    pub builder: &'a mut Builder<'a>,
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub loops: &'a mut Vec<Loop>,
    pub ast: &'a ast::Data,
}

impl<'a> FuncBuilder<'a> {
    pub fn build(&mut self) -> Result {
        let func_ent = &mut self.builder.func_ent();
        let ast = func_ent.ast;
        let ret = func_ent.sig.ret.expand();
        let &body_ast = self.ast.children(ast).last().unwrap();
        
        if body_ast.is_reserved_value() {
            func_ent.kind = func::Kind::External;
            return Ok(());
        }

        let entry_point = self.builder.create_block();
        self.build_function_args(entry_point, ast)?;
        self.builder.select_block(entry_point);

        let expr = self.build_block(body_ast)?;
        
        if !self.builder.is_closed() {
            match (ret, expr) {
                (Some(ty), Some(expr)) => {
                    self.expect_expr_ty(expr, ty)?;
                    {
                        let kind = inst::Kind::Return;
                        let span = self.ast.nodes[body_ast].span;
                        self.builder.add_inst(kind, expr, span);
                    }
                },
                (Some(_), None) => return Err(Error::new(
                    error::Kind::ExpectedValue,
                    self.ast.nodes[body_ast].span,
                )),
                (None, Some(_)) |
                (None, None) => (),
            }
        }

        Ok(())
    }

    fn build_function_args(&mut self, entry_point: Block, ast: Ast) -> Result {
        let children = self.ast.children(ast);
        let params =
            &children[Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_ARG_END];

        self.scope.mark_frame();
        for &param in params {
            let children = self.ast.children(param);
            let ty = self.parse_type(children[children.len() - 1])?;
            for &name in &children[0..children.len() - 1] {
                let name_span = self.ast.nodes[name].span;
                let name_str = self.sources.display(name_span);
                let value = self.builder.add_value(ty, name_span);
                self.scope
                    .push_item(name_str, scope::Item::new(value, name_span));
                self.builder.push_block_param(entry_point, value);
            }
        }

        Ok(())
    }

    fn build_block(&mut self, block_ast: Ast) -> Result<Option<Value>> {
        self.scope.mark_frame();

        let mut value = None;
        for &stmt in self.ast.children(block_ast) {
            value = self.build_stmt(stmt)?;
            if self.builder.is_closed() {
                // TODO: emit warning
                break;
            }
        }

        self.scope.pop_frame();

        Ok(value)
    }

    fn build_stmt(&mut self, stmt: Ast) -> Result<Option<Value>> {
        let ast::Ent { kind, .. } = self.ast.nodes[stmt];
        match kind {
            ast::Kind::Return => self.build_return(stmt),
            _ => self.build_optional_expr(stmt),
        }
    }

    fn build_return(&mut self, stmt: Ast) -> Result<Option<Value>> {
        let value = {
            let children = self.ast.children(stmt);
            let ast = children[0];
            let span = self.ast.nodes[ast].span;
            
            let return_type = self.builder.func_ent().sig.ret.expand();
            let return_value = self.build_optional_expr(ast)?;

            match (return_type, return_value) {
                (None, Some(_)) => {
                    return Err(Error::new(error::Kind::UnexpectedValue, span));
                },
                (Some(_), None) => {
                    return Err(Error::new(error::Kind::ExpectedValue, span));
                },
                (Some(expected), Some(actual)) => {
                    self.expect_expr_ty(actual, expected)?;
                    Some(actual)
                },
                (None, None) => None,
            }
        };

        {
            let span = self.ast.nodes[stmt].span;
            self.builder.add_inst(inst::Kind::Return, value, span);
        }

        Ok(value)
    }

    fn build_expr(&mut self, ast: Ast) -> Result<Value> {
        self.build_optional_expr(ast)?
            .ok_or_else(|| Error::new(error::Kind::ExpectedValue, self.ast.nodes[ast].span))
    }

    fn build_optional_expr(&mut self, ast: Ast) -> Result<Option<Value>> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        match kind {
            ast::Kind::Int(_) | ast::Kind::String | ast::Kind::Bool(_) => self.build_literal(ast).map(Some),
            ast::Kind::Call => self.build_call(ast),
            ast::Kind::Binary => self.build_binary(ast),
            ast::Kind::Ident => self.build_ident(ast).map(Some),
            ast::Kind::If => self.build_if(ast),
            ast::Kind::Variable => self.build_variable(ast),
            ast::Kind::Loop => self.build_loop(ast),
            ast::Kind::Break => self.build_break(ast),
            ast::Kind::Block => self.build_block(ast),
            kind => todo!(
                "Unhandled expression {:?}:\n{}",
                kind,
                SpanDisplay::new(self.sources[span.source()].content(), span),
            ),
        }
    }

    fn build_break(&mut self, ast: Ast) -> Result<Option<Value>> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!();
        };

        let &Loop { end, .. } = self.loops.last()
            .ok_or_else(|| Error::new(error::Kind::InvalidBreak, span))?;

        if value.is_reserved_value() {
            let kind = inst::Kind::Jump(end);
            self.builder.add_inst(kind, None, span);
        } else {
            let kind = inst::Kind::Jump(end);
            let value = self.build_expr(value)?;
            self.builder.add_inst(kind, value, span);
            
            let loop_header = self.loops.last_mut().unwrap();
            if let Some(ty) = loop_header.ty {
                self.expect_expr_ty(value, ty)?;
            } else {
                let ty = self.builder.funcs.values[value].ty;
                loop_header.ty = ty.into();
            }
        }

        Ok(None)
    }

    fn build_loop(&mut self, ast: Ast) -> Result<Option<Value>> {
        let span = self.ast.nodes[ast].span;
        
        let &[body] = self.ast.children(ast) else {
            unreachable!();
        };
        
        let start = self.builder.create_block();
        let end = self.builder.create_block();

        let loop_header = Loop {
            _start: start,
            end,
            ty: None,
        };

        self.loops.push(loop_header);

        self.builder.add_inst(inst::Kind::Jump(start), None, span);

        self.builder.select_block(start);
        self.build_block(body)?;
        if !self.builder.is_closed() {
            self.builder.add_inst(inst::Kind::Jump(start), None, span);
        }
        
        self.builder.select_block(end);

        let loop_header = self.loops.pop().unwrap();

        let mut result = None;
        if let Some(ty) = loop_header.ty {
            let value = self.builder.add_value(ty, span);
            self.builder.push_block_param(loop_header.end, value);
            result = Some(value);
        }

        Ok(result)
    }

    fn build_variable(&mut self, ast: Ast) -> Result<Option<Value>> { // always none
        let &[name, value] = self.ast.children(ast) else {
            unreachable!();
        };

        let id: ID = {
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            str.into()
        };

        let value = self.build_expr(value)?;
        let span = self.ast.nodes[ast].span;

        {
            let span = self.ast.nodes[ast].span;
            self.builder.add_inst(inst::Kind::Variable, value, span);
        }

        self.scope.push_item(id, scope::Item::new(value, span));

        Ok(None)
    } 

    fn build_if(&mut self, ast: Ast) -> Result<Option<Value>> {
        let span = self.ast.nodes[ast].span;

        let &[cond, then, otherwise] = self.ast.children(ast) else {
            unreachable!();
        };

        let cond = self.build_expr(cond)?;
        let bool = self.scope.get::<Ty>("bool", Span::default()).unwrap();
        self.expect_expr_ty(cond, bool)?;
        
        let then_block = self.builder.create_block();
        let otherwise_block = if otherwise.is_reserved_value() {
            None
        } else {
            Some(self.builder.create_block())
        };
        let skip_block = self.builder.create_block();

        {
            let target = otherwise_block.unwrap_or(skip_block);
            let kind = inst::Kind::JumpIfFalse(target);
            self.builder.add_inst(kind, Some(cond), span);
        };

        {
            let kind = inst::Kind::Jump(then_block);
            self.builder.add_inst(kind, None, span);
        }

        self.builder.select_block(then_block);
        let then_value = self.build_block(then)?;
        let mut then_jump = None;
        if !self.builder.is_closed() {
            then_jump = {
                let kind = inst::Kind::Jump(skip_block);
                self.builder.add_inst(kind, None, span).into()
            };
        }

        let mut otherwise_value = None;
        let mut otherwise_jump = None;
        if let Some(otherwise_block) = otherwise_block {
            self.builder.select_block(otherwise_block);
            otherwise_value = self.build_block(otherwise)?;
            if !self.builder.is_closed() {
                otherwise_jump = {
                    let kind = inst::Kind::Jump(skip_block);
                    self.builder.add_inst(kind, None, span).into()
                };
            }
        }

        self.builder.select_block(skip_block);

        match (then_value, otherwise_value) {
            // here we forward the expression
            (Some(then_expr), Some(else_expr)) => { 
                let then_ty = self.builder.funcs.values[then_expr].ty;
                let else_ty = self.builder.funcs.values[else_expr].ty;

                if then_ty != else_ty {
                    return Ok(None);
                }

                if let Some(then_jump) = then_jump {
                    self.builder.funcs.insts[then_jump].value = then_expr.into();
                }

                if let Some(otherwise_jump) = otherwise_jump {
                    self.builder.funcs.insts[otherwise_jump].value = else_expr.into();
                }

                let skip_param = {
                    let span = self.ast.nodes[ast].span;
                    let ent = value::Ent::new(then_ty, span);
                    self.builder.funcs.values.push(ent)
                };
                self.builder.push_block_param(skip_block, skip_param);

                Ok(Some(skip_param))
            },
            _ => Ok(None),
        }
    }

    fn build_ident(&mut self, ast: Ast) -> Result<Value> {
        let span = self.ast.nodes[ast].span;
        let str = self.sources.display(span);
        self.scope.get::<Value>(str, span)
            .map_err(Convert::convert)
    }

    fn build_binary(&mut self, ast: Ast) -> Result<Option<Value>> {
        let &[left, op, right] = self.ast.children(ast) else {
            unreachable!();
        };

        let left = self.build_expr(left)?;
        let right = self.build_expr(right)?;

        let id = {
            let op_id: ID = {
                let span = self.ast.nodes[op].span;
                let str = self.sources.display(span);

                if str == "=" {
                    return self.build_assign(left, right, span).map(Some);
                }

                str.into()
            };
    
            let left_id = {
                let ty = self.builder.funcs.values[left].ty;
                self.types.ents[ty].id
            };
            
            ID::new("<binary>") + op_id + left_id
        };

        let func = {
            let span = self.ast.nodes[op].span;
            self.scope.get::<Func>(id, span).map_err(Convert::convert)?
        };

        let value = {
            let sig = self.builder.funcs.ents[func].sig;
            if let Some(ty) = sig.ret.expand() {
                let span = self.ast.nodes[ast].span;
                let ent = value::Ent::new(ty, span);
                Some(self.builder.funcs.values.push(ent))
            } else {
                None
            }
        };

        {
            let span = self.ast.nodes[ast].span;
            let args = self.builder.funcs.value_slices.list(&[left, right]);
            self.builder.add_inst(inst::Kind::Call(func, args), value, span);
        }

        Ok(value)
    }

    fn build_assign(&mut self, left: Value, right: Value, span: Span) -> Result<Value> {
        let kind = inst::Kind::Assign(left);
        self.builder.add_inst(kind, Some(right), span); 
        return Ok(left);
    }

    fn build_call(&mut self, ast: Ast) -> Result<Option<Value>> {
        let children = self.ast.children(ast);
        let span = self.ast.nodes[ast].span;
        
        let called_func = {
            let ast = children[0];
            let span = self.ast.nodes[ast].span;
            let name = self.sources.display(span);
            self.scope.get::<Func>(name, span).map_err(Convert::convert)?
        };
        
        let args = {
            let sig_args = {
                let args = self.builder.funcs.ents[called_func].sig.args;
                self.types.cons.view(args).to_owned() // TODO: avoid in the future
            };

            if sig_args.len() != children.len() - 1 {
                let kind = error::Kind::ArgCountMismatch(children.len() - 1, sig_args.len());
                return Err(Error::new(kind, span));
            }

            let mut args = vec![];
            for (&arg, ty) in children[1..].iter().zip(sig_args) {
                let expr = self.build_expr(arg)?;
                self.expect_expr_ty(expr, ty)?;
                args.push(expr);
            }
            self.builder.funcs.value_slices.list(&args)
        };

        let return_value = {
            let ty = self.builder.funcs.ents[called_func].sig.ret;
            ty.expand()
                .map(|ty| self.builder.add_value(ty, span))
        };

        {
            let kind = inst::Kind::Call(called_func, args);
            self.builder.add_inst(kind, return_value, span);
        }

        Ok(return_value)
    }

    fn build_literal(&mut self, ast: Ast) -> Result<Value> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        let (ty, kind) = match kind {
            ast::Kind::Int(base) => match base {
                _ => ("int", inst::Kind::IntLit),
            },
            ast::Kind::Bool(b) => ("bool", inst::Kind::BoolLit(b)),
            kind => todo!(
                "Unhandled literal {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        };
        let ty = self.scope.get(ty, span).unwrap();
        let value = self.builder.add_value(ty, span);
        self.builder.add_inst(kind, value, span);
        Ok(value)
    }

    fn expect_expr_ty(&self, expr: Value, expected: Ty) -> Result {
        let value::Ent { ty, span, .. } = self.builder.funcs.values[expr];
        self.expect_ty(ty, expected, span)
    }

    fn expect_ty(&self, ty: Ty, expected: Ty, span: Span) -> Result {
        if ty == expected {
            Ok(())
        } else {
            Err(Error::new(error::Kind::TypeMismatch(ty, expected), span))
        }
    }

    pub fn parse_type(&mut self /* mut on purpose */, ty: Ast) -> Result<Ty> {
        parse_type(self.scope, self.ast, self.sources, ty)
    }
}

pub struct Loop {
    _start: Block,
    end: Block,
    ty: Option<Ty>,
}