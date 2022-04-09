use cranelift_entity::packed_option::ReservedValue;
use lexer::{Sources, SourcesExt, Span, ID};
use modules::{scope::{self, Scope}, module::{Module, self}, logic::Modules};
use parser::{
    ast::{self, Ast},
    {Convert, Parser},
};

use crate::{
    error::{Error, self},
    func::{self, Func, Functions, Signature},
    tir::{
        block::Block,
        inst,
        value::{self, Value},
    },
    ty::{Ty, Types},
};

type Result<T = ()> = std::result::Result<T, Error>;

pub struct Collector<'a> {
    pub scope: &'a mut Scope,
    pub functions: &'a mut Functions,
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

        let args = self.types.add_slice(&args);

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
        let func = self.functions.add(ent);
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
    pub functions: &'a mut Functions,
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub func: Func,
}

impl<'a> FuncBuilder<'a> {
    pub fn build(&mut self) -> Result {
        let func_ent = &mut self.functions.ents[self.func];
        let ast = func_ent.ast;
        let &body_ast = self.ast.children(ast).last().unwrap();
        
        if body_ast.is_reserved_value() {
            func_ent.kind = func::Kind::External;
            return Ok(());
        }

        let entry_point = self.functions.create_block(self.func);
        self.build_function_args(entry_point, ast)?;
        self.functions.select_block(self.func, entry_point);

        self.build_block(body_ast)?;

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
                let value = self.functions.values.push(value::Ent::new(ty, name_span));
                self.scope
                    .push_item(name_str, scope::Item::new(value, name_span));
                self.functions.push_block_param(entry_point, value);
            }
        }

        Ok(())
    }

    fn build_block(&mut self, block_ast: Ast) -> Result<Option<Value>> {
        let mut value = None;
        for &stmt in self.ast.children(block_ast) {
            value = self.build_stmt(stmt)?;
        }

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
            
            let return_type = self.functions.ents[self.func].sig.ret.expand();
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
            let ent = inst::Ent::new(inst::Kind::Return, value, span);
            self.functions.add_inst(self.func, ent);
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
            kind => todo!(
                "Unhandled expression {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }

    fn build_if(&mut self, ast: Ast) -> Result<Option<Value>> {
        let &[cond, then, otherwise] = self.ast.children(ast) else {
            unreachable!();
        };

        let cond = self.build_expr(cond)?;
        let bool = self.scope.get::<Ty>("bool", Span::default()).unwrap();
        self.expect_expr_ty(cond, bool)?;

        let otherwise_block = if otherwise.is_reserved_value() {
            None
        } else {
            Some(self.functions.create_block(self.func))
        };

        let skip_block = self.functions.create_block(self.func);
        let skip = {
            let span = self.ast.nodes[ast].span;
            let target = otherwise_block.unwrap_or(skip_block);
            let kind = inst::Kind::JumpIfFalse(target);
            inst::Ent::new(kind, Some(cond), span)
        };
        self.functions.add_inst(self.func, skip);

        let then_block = self.functions.create_block(self.func);
        let jump = {
            let span = self.ast.nodes[ast].span;
            let kind = inst::Kind::Jump(then_block);
            inst::Ent::new(kind, None, span)
        };
        self.functions.add_inst(self.func, jump);

        self.functions.select_block(self.func, then_block);
        let then_value = self.build_block(then)?;
        let join = {
            let span = self.ast.nodes[ast].span;
            let kind = inst::Kind::Jump(skip_block);
            inst::Ent::new(kind, None, span)
        };
        let then_jump = self.functions.add_inst(self.func, join);

        let mut otherwise_value = None;
        let mut otherwise_jump = None;
        if let Some(otherwise_block) = otherwise_block {
            self.functions.select_block(self.func, otherwise_block);
            otherwise_value = self.build_block(otherwise)?;
            let join = {
                let span = self.ast.nodes[ast].span;
                let kind = inst::Kind::Jump(skip_block);
                inst::Ent::new(kind, None, span)
            };
            otherwise_jump = self.functions.add_inst(self.func, join).into();
        }

        self.functions.select_block(self.func, skip_block);

        match (then_value, otherwise_value) {
            // here we forward the expression
            (Some(then_expr), Some(else_expr)) => { 
                let then_ty = self.functions.values[then_expr].ty;
                let else_ty = self.functions.values[else_expr].ty;

                if then_ty != else_ty {
                    return Ok(None);
                }

                self.functions.insts[then_jump].result = then_expr.into();
                self.functions.insts[otherwise_jump.unwrap()].result = else_expr.into();

                let skip_param = {
                    let span = self.ast.nodes[ast].span;
                    let ent = value::Ent::new(then_ty, span);
                    self.functions.values.push(ent)
                };
                self.functions.push_block_param(skip_block, skip_param);

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
                str.into()
            };
    
            let left_id = {
                let ty = self.functions.values[left].ty;
                self.types.get(ty).id
            };
            
            ID::new("<binary>") + op_id + left_id
        };

        let func = {
            let span = self.ast.nodes[op].span;
            self.scope.get::<Func>(id, span).map_err(Convert::convert)?
        };

        let value = {
            let sig = self.functions.ents[func].sig;
            if let Some(ty) = sig.ret.expand() {
                let span = self.ast.nodes[ast].span;
                let ent = value::Ent::new(ty, span);
                Some(self.functions.values.push(ent))
            } else {
                None
            }
        };

        let inst = {
            let span = self.ast.nodes[ast].span;
            let args = self.functions.make_values([left, right].iter().cloned());
            inst::Ent::new(inst::Kind::Call(func, args), value, span)
        };

        self.functions.add_inst(self.func, inst);

        Ok(value)
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
                let args = self.functions.ents[called_func].sig.args;
                self.types.slice(args).to_owned() // TODO: avoid in the future
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
            self.functions.make_values(args.into_iter())
        };

        let return_value = {
            let ty = self.functions.ents[called_func].sig.ret;
            ty.expand()
                .map(|ty| self.functions.add_value(value::Ent::new(ty, span)))
        };

        {
            let kind = inst::Kind::Call(called_func, args);
            let ent = inst::Ent::new(kind, return_value, span);
            self.functions.add_inst(self.func, ent);
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
        let value = value::Ent::new(ty, span);
        let value = self.functions.add_value(value);
        self.functions
            .add_inst(self.func, inst::Ent::new(kind, Some(value), span));
        Ok(value)
    }

    fn expect_expr_ty(&self, expr: Value, expected: Ty) -> Result {
        let value::Ent { ty, span, .. } = self.functions.values[expr];
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

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use lexer::{SourceEnt, Span, ID};

    use crate::ty::Types;

    use super::*;

    #[test]
    fn test_collect_items() {
        let mut scope = Scope::new();
        let mut sources = Sources::new();
        let mut functions = Functions::new();
        let mut types = Types::new();
        let mut modules = Modules::new();

        let test_str = "
        fn main() -> int {
            ret 0
        }
        ";

        let source = sources.push(SourceEnt::new(PathBuf::from(""), test_str.to_string()));
        scope
            .insert(
                source,
                "int",
                scope::Item::new(Ty(0), Span::new(source, 0, 0)),
            )
            .unwrap();
        
        let module = module::Ent::new(ID::default());
        let module = modules.push(module);

        let mut ast_data = ast::Data::new();
        let mut ast_temp = ast::Temp::new();

        let inter_state =
            Parser::parse_imports(test_str, &mut ast_data, &mut ast_temp, source).unwrap();
        Parser::parse_code_chunk(test_str, &mut ast_data, &mut ast_temp, inter_state).unwrap();

        Collector {
            scope: &mut scope,
            functions: &mut functions,
            types: &mut types,
            modules: &mut modules,
            sources: &sources,
            ast: &ast_data,
        }
        .collect_items(module)
        .unwrap();

        let func = scope.get::<Func>("main", Span::default()).unwrap();

        FuncBuilder {
            scope: &mut scope,
            functions: &mut functions,
            types: &mut types,
            sources: &sources,
            ast: &ast_data,
            func,
        }
        .build()
        .unwrap();

        println!("{}", functions.display(func, &sources, &ast_data));
    }
}
