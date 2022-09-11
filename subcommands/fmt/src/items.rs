use diags::*;
use lexing_t::*;
use parsing::*;
use parsing_t::*;
use scope::*;
use std::{fmt::Write, mem, ops::Range, rc::Rc};
use storage::*;

use crate::Lexer;

#[derive(Default)]
pub struct Fmt {
    parse_state: ParserState,
    ast_data: Rc<AstData>,
    workspace: Workspace,
    buffer: String,
    source: String,
    line_mapping: LineMapping,
    checkpoints: Vec<(usize, usize)>,
    last_newline: usize,
    indent: usize,
    wrap: bool,
}

macro_rules! write {
    ($self:expr, $str:literal $(, $expr:expr)* $(,)?) => {
        {
            std::write!($self.buffer, $str $(, $expr)*).unwrap();
            //$self.check_new_line($str);
        }
    };
}

impl Fmt {
    const LINE_LENGTH: usize = 80;

    pub fn new() -> Self {
        Self::default()
    }

    pub fn source(&mut self, source: String, path: Ident) -> (Option<&str>, String) {
        self.clear(&source, path);

        self.source = source;

        if self.imports() {
            return (None, mem::take(&mut self.source));
        };

        loop {
            let ast_data = Rc::get_mut(&mut self.ast_data).expect("we are the only owner");

            ast_data.clear();
            let errors = self.workspace.error_count();
            let (items, span, finished) = Parser::new(
                &self.source,
                &mut self.parse_state,
                ast_data,
                &mut self.workspace,
            )
            .parse_items_spanned();

            if self.workspace.got_errors_since(errors) {
                break (None, mem::take(&mut self.source));
            }

            self.items(items, span);

            if finished {
                break (Some(&self.buffer), mem::take(&mut self.source));
            } else {
                write!(self, "break\n\n");
            }
        }
    }

    fn imports(&mut self) -> bool {
        let error_count = self.workspace.error_count();

        let ast_data = Rc::get_mut(&mut self.ast_data).expect("we are the only owner");
        let imports = Parser::new(
            &self.source,
            &mut self.parse_state,
            ast_data,
            &mut self.workspace,
        )
        .parse_imports();

        if let Some(imports) = imports {
            self.imports_ast(imports);
        }

        self.workspace.got_errors_since(error_count)
    }

    fn imports_ast(&mut self, imports: Ast) {
        write!(self, "use ");
        let imports_slice = &self.ast_data.clone()[imports.children];
        self.list_low(
            imports.span,
            imports_slice,
            "{",
            "}",
            "",
            true,
            true,
            Self::import,
        );
    }

    fn import(&mut self, ast: Ast) {
        let [alias, path] = self.ast_data[ast.children] else {
            unreachable!();
        };

        if alias.kind != AstKind::None {
            self.ident(alias);
            write!(self, " ");
        }

        self.ident(path);
    }

    fn clear(&mut self, source: &str, path: Ident) {
        self.buffer.clear();
        self.parse_state.start(source, path, false);
        self.line_mapping = LineMapping::new(source);
    }

    pub fn into_workspace(self) -> Workspace {
        self.workspace
    }

    fn items(&mut self, items: VSlice<Ast>, span: Span) {
        let list = &self.ast_data.clone()[items];
        self.list_low(span, list, "", "", "", true, false, Self::item);
    }

    fn item(&mut self, item: Ast) {
        match item.kind {
            AstKind::Struct { vis } => self.struct_decl(item, vis),

            // AstKind::Bound { vis } => todo!(),
            // AstKind::BoundType { vis } => todo!(),
            // AstKind::BoundImpl { vis } => todo!(),
            // AstKind::Impl { vis } => todo!(),
            // AstKind::Func { vis } => todo!(),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn struct_decl(&mut self, item: Ast, vis: Vis) {
        write!(self, "struct ");
        self.vis(vis, false, true);

        let [generics, name, body] = self.ast_data[item.children] else {
            unreachable!()
        };

        self.generics(generics);

        self.ident(name);

        self.struct_body(body);
    }

    fn struct_body(&mut self, body: Ast) {
        let fields = &self.ast_data.clone()[body.children];
        self.list_low(
            body.span,
            fields,
            " {",
            "}",
            "",
            true,
            true,
            Self::struct_field,
        );
    }

    fn struct_field(&mut self, field: Ast) {
        let AstKind::StructField { vis, mutable, exported } = field.kind else {
            unreachable!()
        };

        self.vis(vis, false, true);
        self.exported(exported);
        self.mutable(mutable);

        let [name, ty] = self.ast_data[field.children] else {
            unreachable!()
        };

        self.ident(name);
        write!(self, ": ");
        self.ty(ty);
    }

    fn generics(&mut self, generics: Ast) {
        let list = &self.ast_data.clone()[generics.children];
        if list.is_empty() {
            return;
        }
        self.list(generics.span, list, "[", "] ", ",", |s, node| {
            s.generic(node)
        })
    }

    fn generic(&mut self, item: Ast) {
        let [name, ref bounds @ ..] = self.ast_data.clone()[item.children] else {
            unreachable!()
        };

        self.checkpoint();

        self.ident(name);
        let span = Span::new(name.span.end()..item.span.end());
        self.switch(
            span,
            |s| s.list_low(span, bounds, ": ", "", " + ", false, false, Self::bound),
            |s| s.list_low(span, bounds, ":", "", " +", true, true, Self::bound),
        );
    }

    fn bound(&mut self, bound: Ast) {
        match bound.kind {
            AstKind::Ident | AstKind::IdentChain => self.ident(bound),
            AstKind::TyInstance | AstKind::BoundInstance => self.bound_instance(bound),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn bound_instance(&mut self, bound: Ast) {
        self.ty_instance(bound);
    }

    fn ty(&mut self, ty: Ast) {
        match ty.kind {
            AstKind::Ident | AstKind::IdentChain => self.ident(ty),
            AstKind::TyInstance => self.ty_instance(ty),
            AstKind::PointerTy => self.ty_pointer(ty),
            AstKind::PointerMut => write!(self, "mut"),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn ty_pointer(&mut self, ty: Ast) {
        let [mutability, base] = self.ast_data[ty.children] else {
            unreachable!()
        };

        write!(self, "^");

        if mutability.kind != AstKind::None {
            self.ty(mutability);
            write!(self, " ");
        }

        self.ty(base);
    }

    fn ty_instance(&mut self, ty: Ast) {
        let [name, ref args @ ..] = self.ast_data.clone()[ty.children] else {
            unreachable!()
        };

        self.ident(name);
        self.list(
            Span::new(name.span.end()..ty.span.end()),
            args,
            "[",
            "]",
            ",",
            Self::ty,
        );
    }

    fn ident(&mut self, item: Ast) {
        write!(self, "{}", &self.source[item.span.range()]);
    }

    fn list(
        &mut self,
        span: Span,
        ast: &[Ast],
        start: &str,
        end: &str,
        sep: &str,
        f: impl Fn(&mut Self, Ast) + Copy,
    ) {
        self.switch(
            span,
            |s| s.list_low(span, ast, start, end, sep, false, false, f),
            |s| s.list_low(span, ast, start, end, sep, true, true, f),
        )
    }

    fn list_low(
        &mut self,
        span: Span,
        ast: &[Ast],
        start: &str,
        end: &str,
        sep: &str,
        wrap: bool,
        indent: bool,
        f: impl Fn(&mut Self, Ast),
    ) {
        let prev_wrap = self.wrap;
        self.wrap = wrap;
        let [first, ref rest @ ..] = ast else {
            write!(self, "{}", start);
            self.between(span.range());
            write!(self, "{}", end);
            return;
        };

        write!(self, "{}", start);
        if wrap && indent {
            self.indent();
        }

        if wrap && self.between(span.start()..first.span.start()) {
            self.newline();
            self.write_indent();
        }

        f(self, *first);

        let mut prev = first.span;
        for node in rest {
            if wrap {
                write!(self, "{}", sep);
                if self.between(prev.end()..node.span.start()) {
                    self.newline();
                    self.write_indent();
                }
            } else {
                write!(self, "{} ", sep);
                self.between(prev.end()..node.span.start());
            }
            f(self, *node);
            prev = node.span;
        }

        if wrap {
            write!(self, "{}", sep);
            let should_newline = self.between(prev.end()..span.end());
            if indent {
                self.unindent();
            }
            if should_newline {
                self.newline();
                self.write_indent();
            } else {
                self.buffer.pop().unwrap();
            }
        }

        write!(self, "{}", end);
        self.wrap = prev_wrap;
    }

    fn switch(&mut self, region: Span, linear: impl Fn(&mut Self), wrapped: impl Fn(&mut Self)) {
        self.checkpoint();
        let prev = self.wrap;
        let contains_line_comments =
            Lexer::new(&self.source[region.range()]).contains_line_comments();
        self.wrap = contains_line_comments;
        if self.wrap {
            wrapped(self);
        } else {
            linear(self);
        }
        self.wrap = prev;
        if self.wrap && self.too_far() && !contains_line_comments {
            self.revert();
            self.wrap = true;
            wrapped(self);
            self.wrap = prev;
        } else {
            self.discard_checkpoint();
        }
    }

    fn between(&mut self, range: Range<usize>) -> bool {
        let str = &self.source[range];
        let lexer = Lexer::new(str);
        let prev_newline = self.last_newline;
        lexer.translate(
            &mut self.buffer,
            &mut self.last_newline,
            self.indent,
            self.wrap,
        );
        self.last_newline == prev_newline
    }

    fn checkpoint(&mut self) {
        self.checkpoints
            .push((self.buffer.len(), self.last_newline));
    }

    fn discard_checkpoint(&mut self) {
        self.checkpoints.pop().unwrap();
    }

    fn revert(&mut self) {
        let (checkpoint, last_newline) = self.checkpoints.pop().unwrap();
        self.buffer.truncate(checkpoint);
        self.last_newline = last_newline;
    }

    fn too_far(&self) -> bool {
        self.buffer.len() - self.last_newline > Self::LINE_LENGTH
    }

    fn vis(&mut self, vis: Vis, prev: bool, next: bool) {
        if vis != Vis::None {
            if prev {
                write!(self, " ");
            }
            write!(self, "{}", vis);
            if next {
                write!(self, " ");
            }
        }
    }

    fn mutable(&mut self, mutable: bool) {
        if mutable {
            write!(self, "mut ");
        }
    }

    fn exported(&mut self, exported: bool) {
        if exported {
            write!(self, "use ");
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn unindent(&mut self) {
        self.indent -= 1;
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.buffer.push('\t');
        }
    }

    fn newline(&mut self) {
        self.last_newline = self.buffer.len();
        self.buffer.push('\n');
    }

    // fn check_new_line(&mut self, str: &str) {
    //     if str.contains("\n") {
    //         self.last_newline = self.buffer.rfind('\n').unwrap() + 1;
    //     };
    // }
}
