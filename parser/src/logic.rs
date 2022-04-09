use cranelift_entity::{packed_option::ReservedValue, EntityList};
use lexer::{
    token::{self, Token},
    *,
};

use crate::{
    ast::{self, Ast, Error},
    error,
};

type Result<T = Ast> = std::result::Result<T, ast::Error>;

pub struct Parser<'a> {
    next: Token,
    current: Token,
    lexer: Lexer<'a>,
    ast_file: &'a mut ast::Data,
    temp: &'a mut ast::Temp,
}

impl<'a> Parser<'a> {
    pub const FUNCTION_ARG_START: usize = 2;
    pub const FUNCTION_ARG_END: usize = 2;

    pub fn parse_imports(
        source_str: &'a str,
        ast_file: &'a mut ast::Data,
        temp: &'a mut ast::Temp,
        source: Source,
    ) -> Result<InterState> {
        let inter_state = InterState::new(source);
        Self::parse_with(source_str, ast_file, temp, inter_state, Self::take_imports)
    }

    pub fn parse_manifest(
        source_str: &'a str,
        ast_file: &'a mut ast::Data,
        temp: &'a mut ast::Temp,
        source: Source,
    ) -> Result<()> {
        let inter_state = InterState::new(source);
        Self::parse_with(source_str, ast_file, temp, inter_state, Self::take_manifest).map(|_| ())
    }

    pub fn parse_code_chunk(
        source_str: &'a str,
        ast_file: &'a mut ast::Data,
        temp: &'a mut ast::Temp,
        inter_state: InterState,
    ) -> Result<InterState> {
        Self::parse_with(source_str, ast_file, temp, inter_state, Self::take_chunk)
    }

    pub fn parse_with(
        source_str: &'a str,
        ast_file: &'a mut ast::Data,
        temp: &'a mut ast::Temp,
        InterState {
            mut next,
            mut current,
            progress,
            source,
        }: InterState,
        pfn: impl Fn(&mut Self) -> Result<()>,
    ) -> Result<InterState> {
        let mut lexer = Lexer::new(progress, source_str, source);
        if current.kind() == token::Kind::None {
            current = lexer.next_token();
            next = lexer.next_token();
        }

        let mut s = Self {
            next,
            current,
            lexer,
            ast_file,
            temp,
        };

        pfn(&mut s)?;

        Ok(InterState {
            next: s.next,
            current: s.current,
            progress: s.lexer.progress(),
            source,
        })
    }

    fn take_imports(&mut self) -> Result<()> {
        self.skip_new_lines();
        if self.current.kind() != token::Kind::Use {
            return Ok(());
        }

        let span = self.current.span();

        self.advance();
        self.skip_new_lines();

        self.expect(token::Kind::LeftCurly)?;

        self.temp.mark_frame();
        let end = self.list(
            token::Kind::LeftCurly,
            token::Kind::NewLine,
            token::Kind::RightCurly,
            Self::import,
        )?;
        let imports = self.alloc(ast::Kind::Imports, span.join(end));

        self.ast_file.push(imports);

        Ok(())
    }

    fn take_manifest(&mut self) -> Result<()> {
        self.temp.mark_frame();

        self.list(
            token::Kind::None,
            token::Kind::NewLine,
            token::Kind::Eof,
            Self::constructor_field,
        )?;

        for &item in self.temp.frame_view() {
            self.ast_file.push(item);
        }

        self.temp.pop_frame();

        Ok(())
    }

    fn take_chunk(&mut self) -> Result<()> {
        while self.current.kind() != token::Kind::Eof {
            let item = self.item()?;
            if item.is_reserved_value() {
                break;
            }
            self.ast_file.push(item);
        }

        Ok(())
    }

    pub fn import(&mut self) -> Result {
        let span = self.current.span();
        self.temp.mark_frame();
        if self.current.kind() == token::Kind::Ident {
            let ident = self.ident()?;
            self.temp.acc(ident);
        }

        self.skip_new_lines();
        self.expect(token::Kind::String)?;
        let end = self.current.span();
        let path = self.literal_expr();
        self.temp.acc(path);

        Ok(self.alloc(ast::Kind::Import, span.join(end)))
    }

    pub fn item(&mut self) -> Result {
        loop {
            match self.current.kind() {
                token::Kind::Fn => break self.function(),
                token::Kind::NewLine => self.advance(),
                token::Kind::Eof => break Ok(Ast::reserved_value()),
                _ => todo!(
                    "unhandled {:?} as top level item:\n{}",
                    self.current.kind(),
                    self.lexer.pretty_print(self.current.span())
                ),
            }
        }
    }

    pub fn compute_next(&mut self) {
        if self.next.kind() == token::Kind::None {
            self.advance()
        }
    }

    pub fn advance(&mut self) {
        self.current = self.next;
        self.next = self.lexer.next_token();
    }

    fn function(&mut self) -> Result {
        let span = self.current.span();
        self.advance();

        // call convention
        self.temp.mark_frame();
        if self.current.kind() == token::Kind::String {
            let ast = self
                .ast_file
                .alloc_sonless(ast::Kind::String, self.current.span());
            self.temp.acc(ast);
            self.advance();
        } else {
            self.temp.acc_nil();
        }

        let name = self.ident()?;
        self.temp.acc(name);

        // arguments
        self.list(
            token::Kind::LeftParen,
            token::Kind::Comma,
            token::Kind::RightParen,
            Self::function_argument,
        )?;

        // return type
        if self.current.kind() == token::Kind::RightArrow {
            self.advance();
            let return_type = self.type_expr()?;
            self.temp.acc(return_type);
        } else {
            self.temp.acc_nil();
        }

        let end = if self.current.kind() == token::Kind::LeftCurly {
            let body = self.block()?;
            self.temp.acc(body);
            self.ast_file.nodes[body].span
        } else {
            let end = self.current.span();
            self.advance();
            self.temp.acc_nil();
            end
        };

        Ok(self.alloc(ast::Kind::Function, span.join(end)))
    }

    fn function_argument(&mut self) -> Result {
        let span = self.current.span();

        self.temp.mark_frame();

        // arguments
        self.list(
            token::Kind::None,
            token::Kind::Comma,
            token::Kind::Colon,
            Self::type_expr,
        )?;

        // argument type
        let ty = self.type_expr()?;
        self.temp.acc(ty);

        let end = self.ast_file.nodes[ty].span;

        Ok(self.alloc(ast::Kind::FunctionArgument, span.join(end)))
    }

    fn type_expr(&mut self) -> Result {
        match self.current.kind() {
            token::Kind::Ident => self.type_ident_expr(),
            _ => todo!(
                "unhandled token as type expr:\n{}",
                self.lexer.pretty_print(self.current.span())
            ),
        }
    }

    fn literal_expr(&mut self) -> Ast {
        let span = self.current.span();
        let kind = match self.current.kind() {
            token::Kind::Int(i) => ast::Kind::Int(i),
            token::Kind::String => ast::Kind::String,
            token::Kind::Bool(value) => ast::Kind::Bool(value),
            _ => todo!(
                "unhandled token as literal expr:\n{}",
                self.lexer.pretty_print(self.current.span())
            ),
        };
        self.advance();
        self.ast_file.alloc_sonless(kind, span)
    }

    fn block(&mut self) -> Result {
        let span = self.current.span();
        self.temp.mark_frame();
        let end = self.list(
            token::Kind::LeftCurly,
            token::Kind::NewLine,
            token::Kind::RightCurly,
            Self::expr,
        )?;
        Ok(self.alloc(ast::Kind::Block, span.join(end)))
    }

    fn expr(&mut self) -> Result {
        let prev = self.simple_expr()?;
        if self.current.kind() == token::Kind::Operator {
            let precedence = {
                let span = self.current.span();
                let str = self.lexer.display(span);
                Self::precedence(str)
            };
            self.composite_expr(prev, precedence)
        } else {
            Ok(prev)
        }
    }

    fn composite_expr(&mut self, mut prev: Ast, prev_precedence: usize) -> Result {
        while self.current.kind() == token::Kind::Operator {
            let op = {
                let span = self.current.span();
                let ent = ast::Ent::childless(ast::Kind::Ident, span);
                self.advance();
                self.ast_file.nodes.push(ent)
            };
            
            let mut expr = self.simple_expr()?;

            let precedence = {
                let span = self.current.span();
                let str = self.lexer.display(span);
                Self::precedence(str)
            };

            if precedence < prev_precedence {    
                expr = self.composite_expr(expr, precedence)?;
            }

            let span = {
                let start = self.ast_file.nodes[prev].span;
                let end = self.ast_file.nodes[expr].span;
                start.join(end)
            };

            self.temp.mark_frame();
            self.temp.acc(prev);
            self.temp.acc(op);
            self.temp.acc(expr);

            prev = self.alloc(ast::Kind::Binary, span);
        }

        Ok(prev)
    }

    fn simple_expr(&mut self) -> Result {
        match self.current.kind() {
            token::Kind::Ret => self.return_expr(),
            token::Kind::Ident => self.ident_expr(),
            token::Kind::Int(_) | token::Kind::String | token::Kind::Bool(_) => Ok(self.literal_expr()),
            token::Kind::If => self.if_expr(),
            token::Kind::LeftCurly => self.block(),
            _ => todo!(
                "unhandled token as simple expr:\n{}",
                self.lexer.pretty_print(self.current.span())
            ),
        }
    }

    pub fn if_expr(&mut self) -> Result {
        self.temp.mark_frame();
        let span = self.current.span();
        self.advance();

        let cond = self.expr()?;
        let then = self.block()?;
        let otherwise = if self.current.kind() == token::Kind::Else {
            self.advance();
            self.expr()? // this allows omitting {} but allows 'else if'
        } else {
            Ast::reserved_value()
        };

        self.temp.acc(cond);
        self.temp.acc(then);
        self.temp.acc(otherwise);

        let end = if otherwise.is_reserved_value() {
            self.ast_file.nodes[then].span
        } else {
            self.ast_file.nodes[otherwise].span
        };

        Ok(self.alloc(ast::Kind::If, span.join(end)))
    }

    fn ident_expr(&mut self) -> Result {
        let span = self.current.span();
        self.temp.mark_frame();
        let ident = self.ident()?;

        match self.current.kind() {
            token::Kind::LeftParen => {
                self.temp.acc(ident);
                let end = self.list(
                    token::Kind::LeftParen, 
                    token::Kind::Comma, 
                    token::Kind::RightParen, 
                    Self::expr
                )?;

                return Ok(self.alloc(ast::Kind::Call, span.join(end)))
            },
            token::Kind::LeftBracket => {
                self.temp.acc(ident);
                self.advance();
                let index = self.expr()?;
                self.expect(token::Kind::RightBracket)?;
                let end = self.current.span();
                self.advance();
                self.temp.acc(index);
                return Ok(self.alloc(ast::Kind::Index, span.join(end)))
            },
            _ => {
                self.pop_frame();
            }
        }

        Ok(ident)
    }

    fn return_expr(&mut self) -> Result {
        let span = self.current.span();
        self.advance();
        self.temp.mark_frame();
        let end = if self.current.kind() == token::Kind::NewLine
            || self.current.kind() == token::Kind::Comma
        {
            self.temp.acc_nil();
            Span::default()
        } else {
            let expr = self.expr()?;
            self.temp.acc(expr);
            self.ast_file.nodes[expr].span
        };
        Ok(self.alloc(ast::Kind::Return, span.join(end)))
    }

    fn constructor_field(&mut self) -> Result {
        let span = self.current.span();
        let name = self.ident()?;
        self.temp.bottom(name);

        let (expr, end) = if self.current.kind() == token::Kind::LeftCurly {
            // inferred branch
            self.temp.mark_frame();
            let end = self.list(
                token::Kind::LeftCurly,
                token::Kind::NewLine,
                token::Kind::RightCurly,
                Self::constructor_field,
            )?;
            let expr = self.alloc(ast::Kind::InlineConstructor, end);
            (expr, end)
        } else {
            // normal expr after colon
            self.expect(token::Kind::Colon)?;
            self.advance();
            let expr = self.expr()?;
            (expr, self.ast_file.nodes[expr].span)
        };

        self.temp.acc(expr);
        Ok(self.alloc(ast::Kind::StructField, span.join(end)))
    }

    fn type_ident_expr(&mut self) -> Result {
        let span = self.current.span();
        let name = self.ast_file.alloc_sonless(ast::Kind::Ident, span);
        self.advance();
        self.temp.bottom(name);

        let end = self.list(
            token::Kind::LeftBracket,
            token::Kind::Comma,
            token::Kind::RightBracket,
            Self::type_expr,
        )?;

        Ok(self.alloc(ast::Kind::Ident, span.join(end)))
    }

    fn ident(&mut self) -> Result {
        self.expect(token::Kind::Ident)?;
        let res = self
            .ast_file
            .alloc_sonless(ast::Kind::Ident, self.current.span());
        self.advance();
        Ok(res)
    }

    pub fn list(
        &mut self,
        start: token::Kind,
        sep: token::Kind,
        end: token::Kind,
        function_arg: impl Fn(&mut Self) -> Result,
    ) -> Result<Span> {
        let span = self.current.span();
        if start != token::Kind::None {
            if start != self.current.kind() {
                return Ok(Span::default());
            }
            self.advance();
        }
        self.skip_new_lines();

        if end == self.current.kind() {
            let end = self.current.span();
            self.advance();
            return Ok(span.join(end));
        }

        loop {
            let ast = function_arg(self)?;
            self.temp.acc(ast);
            let end_span = self.ast_file.nodes[ast].span;

            if end == self.current.kind() {
                let end = self.current.span();
                self.advance();
                return Ok(span.join(end));
            } else if end == token::Kind::None && self.current.kind() != sep {
                return Ok(span.join(end_span));
            }

            self.expect(sep)?;
            self.advance();
            self.skip_new_lines();

            if end == self.current.kind() {
                let end = self.current.span();
                self.advance();
                return Ok(span.join(end));
            }
        }
    }

    fn skip_new_lines(&mut self) {
        while self.current.kind() == token::Kind::NewLine {
            self.advance();
        }
    }

    fn expect(&self, ident: token::Kind) -> Result<()> {
        if self.current.kind() == ident {
            Ok(())
        } else {
            // panic!(
            //     "expected token {:?} but got {:?}",
            //     ident,
            //     self.current.kind()
            // );
            Err(Error::new(
                error::Kind::Expected(ident, self.current.kind()),
                self.current.span(),
            ))
        }
    }

    fn alloc(&mut self, kind: ast::Kind, span: Span) -> Ast {
        let cons = self.pop_frame();
        self.ast_file.alloc_ent(ast::Ent::new(kind, cons, span))
    }

    fn pop_frame(&mut self) -> EntityList<Ast> {
        self.temp.save_frame(self.ast_file)
    }

    pub const EQUAL_SIGN_PRECEDENCE: usize = 14;

    pub const INFINITE_PRECEDENCE: usize = usize::MAX;

    pub fn precedence(op: &str) -> usize {
        match op {
            "*" | "/" | "%" => 3,
            "+" | "-" => 4,
            "<<" | ">>" => 5,
            "<" | ">" | "<=" | ">=" => 6,
            "==" | "!=" => 7,
            "&" => 8,
            "^" => 9,
            "|" => 10,
            "&&" => 11,
            "||" => 12,
            _ => if op.ends_with('=') {
                Self::EQUAL_SIGN_PRECEDENCE
            } else {
                Self::INFINITE_PRECEDENCE
            },
        }
    }
}

pub struct InterState {
    next: Token,
    current: Token,
    progress: usize,
    source: Source,
}

impl InterState {
    fn new(source: Source) -> Self {
        Self {
            next: Default::default(),
            current: Default::default(),
            progress: 0,
            source,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Write;

    #[test]
    fn test_code_chunk() {
        let test_string = "
        #repr = \"int\"
        struct int
        
        fn main() -> int {
            ret 0
        }
        ";

        let mut ast_file = ast::Data::new();
        let mut temp = ast::Temp::new();
        let inter_state = InterState::new(Source(0));

        Parser::parse_code_chunk(test_string, &mut ast_file, &mut temp, inter_state).unwrap();

        let mut result = String::new();

        writeln!(result, "{}", ast::FileDisplay::new(&ast_file, test_string)).unwrap();

        std::fs::write("test_out.txt", result).unwrap();
    }

    #[test]
    fn test_manifest() {
        let test_string = "       
        root: \"\"
        dependency {
            a: \"\"
        }
        ";

        let mut ast_file = ast::Data::new();
        let mut temp = ast::Temp::new();

        Parser::parse_manifest(test_string, &mut ast_file, &mut temp, Source(0)).unwrap();

        let mut result = String::new();

        writeln!(result, "{}", ast::FileDisplay::new(&ast_file, test_string)).unwrap();

        let mut f = std::fs::OpenOptions::new()
            .write(true)
            .append(true)
            .open("test_out.txt")
            .unwrap();

        std::io::Write::write_all(&mut f, result.as_bytes()).unwrap();
    }
}
