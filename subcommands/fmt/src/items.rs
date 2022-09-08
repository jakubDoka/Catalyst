use diags::*;
use lexing_t::*;
use parsing::*;
use parsing_t::*;
use scope::*;
use std::{fmt::Write, mem, rc::Rc};
use storage::*;

#[derive(Default)]
pub struct Fmt {
    parse_state: ParserState,
    ast_data: Rc<AstData>,
    workspace: Workspace,
    buffer: String,
    source: String,
    line_mapping: LineMapping,
    checkpoints: Vec<usize>,
    last_newline: usize,
    ident: usize,
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

        if !self.imports() {
            return (None, mem::take(&mut self.buffer));
        };

        loop {
            let ast_data = Rc::get_mut(&mut self.ast_data).expect("we are the only owner");

            ast_data.clear();
            let errors = self.workspace.error_count();
            let (items, finished) = Parser::new(
                &self.source,
                &mut self.parse_state,
                ast_data,
                &mut self.workspace,
            )
            .parse_items();

            if self.workspace.got_errors_since(errors) {
                break (None, mem::take(&mut self.source));
            }

            self.items(items);

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
        self.list_low(imports_slice, "{", "}", "", true, Self::import);
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
        self.parse_state.start(source, path);
        self.line_mapping = LineMapping::new(source);
    }

    pub fn into_workspace(self) -> Workspace {
        self.workspace
    }

    fn items(&mut self, items: VSlice<Ast>) {
        for &item in &self.ast_data.clone()[items] {
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
    }

    fn struct_decl(&mut self, item: Ast, vis: Vis) {
        self.vis(vis);
        write!(self, "struct");

        let [generics, name, body] = self.ast_data[item.children] else {
            unreachable!()
        };

        self.generics(generics);

        self.ident(name);

        self.struct_body(body);
    }

    fn struct_body(&mut self, body: Ast) {
        let fields = &self.ast_data.clone()[body.children];
        self.list_low(fields, " {", "}", "", true, Self::struct_field);
    }

    fn struct_field(&mut self, field: Ast) {
        let AstKind::StructField { vis, mutable, exported } = field.kind else {
            unreachable!()
        };

        self.vis(vis);
        self.mutable(mutable);
        self.exported(exported);

        let [name, ty] = self.ast_data[field.children] else {
            unreachable!()
        };

        self.ident(name);
        write!(self, ": ");
        self.ty(ty);
    }

    fn generics(&mut self, generics: Ast) {
        let list = &self.ast_data.clone()[generics.children];
        self.list(list, " [", "] ", ",", |s, node| s.generic(node))
    }

    fn generic(&mut self, item: Ast) {
        let [name, ref bounds @ ..] = self.ast_data.clone()[item.children] else {
            unreachable!()
        };

        self.checkpoint();

        self.ident(name);
        self.switch(
            |s| s.list_low(bounds, ": ", "", " + ", false, Self::bound),
            |s| s.list_low(bounds, ":", "", " +", true, Self::bound),
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
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn ty_instance(&mut self, ty: Ast) {
        let [name, ref args @ ..] = self.ast_data.clone()[ty.children] else {
            unreachable!()
        };

        self.ident(name);
        self.list(args, "[", "]", ",", Self::ty);
    }

    fn ident(&mut self, item: Ast) {
        write!(self, "{}", &self.source[item.span.range()]);
    }

    fn list(
        &mut self,
        ast: &[Ast],
        start: &str,
        end: &str,
        sep: &str,
        f: impl Fn(&mut Self, Ast) + Copy,
    ) {
        self.switch(
            |s| s.list_low(ast, start, end, sep, false, f),
            |s| s.list_low(ast, start, end, sep, true, f),
        )
    }

    fn list_low(
        &mut self,
        ast: &[Ast],
        start: &str,
        end: &str,
        sep: &str,
        wrap: bool,
        f: impl Fn(&mut Self, Ast),
    ) {
        write!(self, "{}", start);
        if wrap {
            self.indent();
            self.newline();
            self.write_indent();
        }

        if let [first, ref rest @ ..] = ast {
            f(self, *first);
            for node in rest {
                write!(self, "{}", sep);
                if wrap {
                    self.newline();
                    self.write_indent();
                } else {
                    write!(self, "{} ", sep);
                }
                f(self, *node);
            }
        }

        if wrap {
            self.unindent();
            self.newline();
            self.write_indent();
        }

        write!(self, "{}", end);
    }

    fn switch(&mut self, linear: impl Fn(&mut Self), wrapped: impl Fn(&mut Self)) {
        self.checkpoint();
        let prev = self.wrap;
        self.wrap = false;
        linear(self);
        self.wrap = prev;
        if self.wrap && self.too_far() {
            self.revert();
            self.wrap = true;
            wrapped(self);
            self.wrap = prev;
        } else {
            self.discard_checkpoint();
        }
    }

    fn checkpoint(&mut self) {
        self.checkpoints.push(self.buffer.len());
    }

    fn discard_checkpoint(&mut self) {
        self.checkpoints.pop().unwrap();
    }

    fn revert(&mut self) {
        let checkpoint = self.checkpoints.pop().unwrap();
        self.buffer.truncate(checkpoint);
    }

    fn too_far(&self) -> bool {
        self.buffer.len() - self.last_newline > Self::LINE_LENGTH
    }

    fn vis(&mut self, vis: Vis) {
        write!(self, "{}", vis);
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
        self.ident += 1;
    }

    fn unindent(&mut self) {
        self.ident -= 1;
    }

    fn write_indent(&mut self) {
        for _ in 0..self.ident {
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
