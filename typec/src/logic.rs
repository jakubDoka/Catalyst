use std::sync::Arc;

use cranelift_entity::{
    packed_option::ReservedValue,
    EntityList, ListPool, PrimaryMap,
};
use modules::{
    logic::SourceManager,
    scope::{self, Scope},
};
use parser::{
    ast::{self, Ast},
    prelude::{Convert, Parser},
};

use crate::{
    error::Error,
    func::{self, Func, Signature},
    ty::{self, Ty}, ir::{self, block::Block, value::{self, Value}, inst},
};

type Result<T = ()> = std::result::Result<T, Error>;

crate::gen_context!(CollectContext<'a> {
    scope: &'a mut Scope, 
    ast: &'a ast::Data,
});

crate::gen_context!(BuildContext<'a> {
    func: &'a mut ir::Function,
    scope: &'a mut Scope, 
    ast: &'a ast::Data,
});

impl<'a> BuildContext<'a> {
    pub fn as_collect_context<'b>(&'b mut self) -> CollectContext<'b> {
        CollectContext {
            scope: self.scope,
            ast: self.ast,
        }
    }
}

pub struct TypeManager {
    sources: SourceManager,
    types: PrimaryMap<Ty, ty::Ent>,
    ty_cons: ListPool<Ty>,
    funcs: PrimaryMap<Func, func::Ent>,
}

impl TypeManager {
    pub fn new() -> Self {
        TypeManager {
            sources: SourceManager::new(),
            types: PrimaryMap::new(),
            ty_cons: ListPool::new(),
            funcs: PrimaryMap::new(),
        }
    }

    pub fn add_type(&mut self, ty: ty::Ent) -> Ty {
        self.types.push(ty)
    }

    pub fn build_function_ir(&mut self, func: Func, ctx: &mut BuildContext) -> Result {
        let func::Ent { sig, ast } = self.funcs[func]; 
        ctx.func.sig = sig;

        let entry_point = ctx.func.create_block();
        self.build_function_args(entry_point, ast, ctx)?;

        let &body_ast = ctx.ast.children(ast).last().unwrap();

        self.build_block(body_ast, ctx)?;

        Ok(())
    }
    
    fn build_function_args(&mut self, entry_point: Block, ast: Ast, ctx: &mut BuildContext) -> Result {
        let children = ctx.ast.children(ast);        
        let params = &children[Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_ARG_END];
        
        ctx.scope.mark_frame();
        for &param in params {
            let children = ctx.ast.children(param);
            let ty = self.build_type(children[children.len() - 1], ctx)?;
            for &name in &children[0..children.len() - 1] {
                let name_span = ctx.ast.span(name);
                let name_str = self.sources.display(name_span);
                let value = ctx.func.add_value(value::Ent::new(ty, name));
                ctx.scope.push_item(name_str, scope::Item::new(value, name_span));
                ctx.func.push_block_param(entry_point, value);
            }
        }

        Ok(())
    }

    fn build_block(&mut self, block_ast: Ast, ctx: &mut BuildContext) -> Result<Option<Value>> {
        let mut value = None;
        for &stmt in ctx.ast.children(block_ast) {
            value = self.build_stmt(stmt, ctx)?;
        }
        
        Ok(value)
    }

    fn build_stmt(&mut self, stmt: Ast, ctx: &mut BuildContext) -> Result<Option<Value>> {
        match ctx.ast.kind(stmt) {
            ast::Kind::Return => self.build_return(stmt, ctx),
            kind => todo!("Unhandled statement {:?}: {}", kind, self.sources.display(ctx.ast.span(stmt))),
        }
    }

    fn build_return(&mut self, stmt: Ast, ctx: &mut BuildContext) -> Result<Option<Value>> {
        let children = ctx.ast.children(stmt);
        let value = self.build_expr(children[0], ctx)?;
        ctx.func.add_inst(inst::Ent::new(inst::Kind::Return, Some(value), stmt));
        Ok(None)
    }

    fn build_expr(&mut self, ast: Ast, ctx: &mut BuildContext) -> Result<Value> {
        match ctx.ast.kind(ast) {
            ast::Kind::Int(_) | ast::Kind::String => self.build_literal(ast, ctx),
            kind => todo!("Unhandled expression {:?}: {}", kind, self.sources.display(ctx.ast.span(ast))),
        }
    }

    fn build_literal(&mut self, ast: Ast, ctx: &mut BuildContext) -> Result<Value> {        
        let ty = match ctx.ast.kind(ast) {
            ast::Kind::Int(base) => match base {
                _ => "int",
            },
            kind => todo!("Unhandled literal {:?}: {}", kind, self.sources.display(ctx.ast.span(ast))),
        };
        let ty = ctx.scope.get(ty, ctx.ast.span(ast)).unwrap();
        let value = value::Ent::new(ty, ast);
        let value = ctx.func.add_value(value);
        Ok(value)
    }

    fn build_type(&mut self, ast: Ast, ctx: &mut BuildContext) -> Result<Ty> {
        self.parse_type(&mut ctx.as_collect_context(), ast)
    }

    pub fn collect_items(&mut self, ctx: &mut CollectContext) -> Result {
        for (ast, &ast::Ent { kind, span, .. }) in ctx.ast.elements() {
            match kind {
                ast::Kind::Function => self.collect_function(ctx, ast)?,
                _ => todo!("Unhandled top-level item:\n{}", self.sources.display(span)),
            }
        }

        Ok(())
    }

    fn collect_function(&mut self, ctx: &mut CollectContext, ast: Ast) -> Result {
        let children = ctx.ast.children(ast);
        let current_span = ctx.ast.span(ast);
        let &[call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let mut args = EntityList::new();
        for &ast in &children[Parser::FUNCTION_ARG_START..children.len() - Parser::FUNCTION_ARG_END] {
            let children = ctx.ast.children(ast);
            let amount = children.len() - 1;
            let ty = children[amount];
            let ty = self.parse_type(ctx, ty)?;
            for _ in 0..amount {
                args.push(ty, &mut self.ty_cons);
            }
        }

        let ret = if return_type.is_reserved_value() {
            None
        } else {
            Some(self.parse_type(ctx, return_type)?)
        }
        .into();

        let sig = Signature {
            call_conv,
            args,
            ret,
        };
        let ent = func::Ent { sig, ast };
        let func = self.funcs.push(ent);
        ctx.scope
            .insert(
                current_span.source(),
                self.sources.display(ctx.ast.span(name)),
                scope::Item::new(func, current_span),
            )
            .map_err(Convert::convert)?;

        Ok(())
    }

    fn parse_type(&self, ctx: &mut CollectContext, ty: Ast) -> Result<Ty> {
        let &ast::Ent { kind, span, .. } = ctx.ast.get(ty);
        match kind {
            ast::Kind::Ident => {
                return ctx.scope
                    .get(self.sources.display(span), span)
                    .map_err(Convert::convert);
            }
            _ => todo!(
                "Unhandled type expr {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use lexer::prelude::{Span, SourceEnt};
    use parser::prelude::Parser;

    use crate::ir::Function;

    use super::*;

    #[test]
    fn test_collect_items() {
        let mut scope = Scope::new();
        
        let mut type_manager = TypeManager::new();
        let test_str =
        "
        fn foo(a, b: int, c: int) -> int {
            ret 0
        }
        ";
        
        let source = type_manager.sources.add(SourceEnt::new(PathBuf::from(""), test_str.to_string()));
        scope.insert(source, "int", scope::Item::new(Ty(0), Span::new(source, 0, 0))).unwrap();
        
        let mut ast_data = ast::Data::new();
        let mut ast_temp = ast::Temp::new();

        let inter_state = Parser::parse_imports(test_str, &mut ast_data, &mut ast_temp, source).unwrap();
        Parser::parse_code_chunk(test_str, &mut ast_data, &mut ast_temp, inter_state).unwrap();

        type_manager.collect_items(&mut (&mut scope, &ast_data).into()).unwrap();
        let func = scope.get::<Func>("foo", Span::default()).unwrap();
        let mut function = Function::new();

        type_manager.build_function_ir(func, &mut (&mut function, &mut scope, &ast_data).into()).unwrap();

        println!("{}", function.display(&type_manager, &ast_data));
    }
}
