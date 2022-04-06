use cranelift_entity::packed_option::ReservedValue;
use lexer::{Sources, SourcesExt};
use modules::{scope::{self, Scope}, module::{Module, self}, logic::Modules};
use parser::{
    ast::{self, Ast},
    {Convert, Parser},
};

use crate::{
    error::Error,
    func::{self, Func, Functions, Signature},
    tir::{
        block::Block,
        inst,
        value::{self, Value},
    },
    ty::{Ty, Types},
};

type Result<T = ()> = std::result::Result<T, Error>;

crate::gen_context!(Collector<'a> {
    scope: &'a mut Scope,
    functions: &'a mut Functions,
    types: &'a mut Types,
    modules: &'a mut Modules,
    sources: &'a Sources,
    ast: &'a ast::Data,
});

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

        let sig = Signature {
            call_conv: self.ast.nodes[call_conv].span,
            args,
            ret,
        };
        let ent = func::Ent {
            sig,
            ast,
            ..Default::default()
        };
        let func = self.functions.add(ent);
        let name_span = self.ast.nodes[name].span;
        let id = self.sources.display(name_span).into();
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

crate::gen_context!(Builder<'a> {
    functions: &'a mut Functions,
    scope: &'a mut Scope,
    types: &'a mut Types,
    sources: &'a Sources,
    ast: &'a ast::Data,
});

impl<'a> Builder<'a> {
    pub fn build_function_ir(&mut self, func: Func) -> Result {
        let ast = self.functions.get(func).ast;

        let entry_point = self.functions.create_block(func);
        self.build_function_args(entry_point, ast)?;

        let &body_ast = self.ast.children(ast).last().unwrap();

        self.build_block(func, body_ast)?;

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
                let value = self.functions.add_value(value::Ent::new(ty, name_span));
                self.scope
                    .push_item(name_str, scope::Item::new(value, name_span));
                self.functions.push_block_param(entry_point, value);
            }
        }

        Ok(())
    }

    fn build_block(&mut self, func: Func, block_ast: Ast) -> Result<Option<Value>> {
        let mut value = None;
        for &stmt in self.ast.children(block_ast) {
            value = self.build_stmt(func, stmt)?;
        }

        Ok(value)
    }

    fn build_stmt(&mut self, func: Func, stmt: Ast) -> Result<Option<Value>> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[stmt];
        match kind {
            ast::Kind::Return => self.build_return(func, stmt),
            kind => todo!(
                "Unhandled statement {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }

    fn build_return(&mut self, func: Func, stmt: Ast) -> Result<Option<Value>> {
        let children = self.ast.children(stmt);
        let value = self.build_expr(func, children[0])?;
        self.functions.add_inst(
            func,
            inst::Ent::new(inst::Kind::Return, Some(value), self.ast.nodes[stmt].span),
        );
        Ok(None)
    }

    fn build_expr(&mut self, func: Func, ast: Ast) -> Result<Value> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        match kind {
            ast::Kind::Int(_) | ast::Kind::String => self.build_literal(func, ast),
            kind => todo!(
                "Unhandled expression {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }

    fn build_literal(&mut self, func: Func, ast: Ast) -> Result<Value> {
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];
        let (ty, kind) = match kind {
            ast::Kind::Int(base) => match base {
                _ => ("int", inst::Kind::IntLit),
            },
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
            .add_inst(func, inst::Ent::new(kind, Some(value), span));
        Ok(value)
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

        Builder {
            scope: &mut scope,
            functions: &mut functions,
            types: &mut types,
            sources: &sources,
            ast: &ast_data,
        }
        .build_function_ir(func)
        .unwrap();

        println!("{}", functions.display(func, &sources, &ast_data));
    }
}
