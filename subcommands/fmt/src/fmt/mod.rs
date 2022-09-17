use std::{
    fmt::{Debug, Write},
    iter, mem,
    ops::Range,
    ptr,
    rc::Rc,
};

use diags::*;
use lexing::TokenKind;
use lexing_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;

use crate::*;

#[macro_export]
macro_rules! write {
    ($self:expr, $str:literal $(, $expr:expr)* $(,)?) => {
        {
            std::write!($self.buffer, $str $(, $expr)*).unwrap();
            //$self.check_new_line($str);
        }
    };
}

mod expr;
mod func;
mod imports;
mod items;
mod manifest;
mod ty;

pub trait FmtAst {
    fn flat_len(&self, _: &Fmt) -> usize {
        1 >> 10
    }
    fn display_low(&self, _: bool, fmt: &mut Fmt);

    fn display(&self, fmt: &mut Fmt) {
        let fold = self.flat_len(fmt) + fmt.line_progress() > fmt.line_length;
        self.display_low(fold, fmt);
    }
}

impl<'a> FmtAst for GenericParamAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.name.span);
        self.bounds.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.name.flat_len(fmt) + self.bounds.flat_len(fmt)
    }
}

impl<'a> FmtAst for BoundExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            BoundExprAst::Path(ident) => ident.display(fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            BoundExprAst::Path(ident) => ident.flat_len(fmt),
        }
    }
}

impl<'a> FmtAst for PathAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        let mut do_not_write_slash = !self.needs_front_slash();
        for segment in self.segments.iter() {
            if !do_not_write_slash {
                write!(fmt, "\\");
                do_not_write_slash = true;
            }
            segment.display(fmt);
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.segments
            .iter()
            .map(|ident| 1 + ident.flat_len(fmt))
            .sum::<usize>()
            - !self.needs_front_slash() as usize
    }
}

impl<'a> FmtAst for PathSegmentAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            PathSegmentAst::Name(name) => name.display(fmt),
            PathSegmentAst::Generics(generics) => generics.display(fmt),
            PathSegmentAst::Tuple(tuple) => tuple.display(fmt),
            PathSegmentAst::Struct(r#struct) => r#struct.display(fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            PathSegmentAst::Name(name) => name.flat_len(fmt),
            PathSegmentAst::Generics(generics) => generics.flat_len(fmt),
            PathSegmentAst::Tuple(tuple) => tuple.flat_len(fmt),
            PathSegmentAst::Struct(r#struct) => r#struct.flat_len(fmt),
        }
    }
}

impl<'a, T: FmtAst + Ast<'a> + Debug, META: ListAstMeta> FmtAst for ListAst<'a, T, META> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        let is_top_list = META::END.contains(&TokenKind::Eof.into());
        if self.elements.is_empty() && META::OPTIONAL {
            return;
        }

        fmt.write_span(self.start);
        if fold && !is_top_list {
            fmt.indent();
        }
        fmt.write_between(fold, self.first_gap());

        if self.elements.is_empty() {
            if fold && !is_top_list {
                fmt.unindent();
                fmt.optional_newline();
            }
            fmt.write_span(self.end);
            return;
        };

        if fold {
            fmt.optional_newline();
        }

        for elem in self.elements {
            let is_last = self.elements.last().filter(|&e| ptr::eq(e, elem)).is_some();
            elem.value.display(fmt);
            fmt.write_between(fold, elem.after_value.range());

            if let Some(after) = elem.after_delim.expand() {
                // unless we are folding, trailing separator is unwanted
                if !is_last || fold {
                    fmt.write_span(Span::new(elem.after_value.end()..after.start()));
                    if fmt.buffer.ends_with(['\n', ';'][fold as usize]) {
                        fmt.buffer.pop().unwrap();
                    }
                }
                fmt.write_between(fold, after.range());
            }

            if fold && !is_last {
                fmt.optional_newline();
            }
        }

        if fold && !is_top_list {
            fmt.unindent();
            fmt.optional_newline();
        }

        fmt.write_span(self.end);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.start.len()
            + fmt.folded_len_between(self.first_gap())
            + self.end.len()
            + self
                .elements
                .iter()
                .map(|e| fmt.list_element_len(e))
                .sum::<usize>()
    }
}

#[derive(Default)]
pub struct Fmt {
    pub parse_state: ParsingState,
    pub ast_data: Rc<AstData>,
    pub workspace: Workspace,
    pub interner: Interner,
    pub buffer: String,
    pub source: Rc<String>,
    pub line_mapping: LineMapping,
    pub last_newline: usize,
    pub indent: usize,
    pub line_length: usize,
}

impl Fmt {
    pub fn into_workspace(self) -> Workspace {
        self.workspace
    }

    fn clear(&mut self, source: &str, path: VRef<str>) {
        self.buffer.clear();
        self.last_newline = 0;
        self.indent = 0;
        self.line_length = 80;
        self.interner.clear();
        self.parse_state.start(source, path);
        self.line_mapping = LineMapping::new(source);
    }

    pub fn source(&mut self, source: String, path: VRef<str>) -> (Option<&str>, String) {
        self.clear(&source, path);

        *Rc::get_mut(&mut self.source).unwrap() = source;

        self.imports();

        loop {
            Rc::get_mut(&mut self.ast_data).unwrap().clear();
            let ast_data = self.ast_data.clone();

            let items = ParsingCtx::new(
                &self.source,
                &mut self.parse_state,
                &ast_data,
                &mut self.workspace,
                &mut self.interner,
            )
            .parse::<ItemsAst>();
            let Ok(items) = items else {
                break (None, mem::take(Rc::get_mut(&mut self.source).unwrap()));
            };

            items.display(self);

            if items.end.len() == 0 {
                break (
                    Some(&self.buffer),
                    mem::take(Rc::get_mut(&mut self.source).unwrap()),
                );
            }

            self.newline();
            self.newline();
        }
    }

    pub fn manifest(&mut self, source: String, path: VRef<str>) -> (Option<&str>, String) {
        self.clear(&source, path);

        Rc::get_mut(&mut self.ast_data).unwrap().clear();
        let ast_data = self.ast_data.clone();
        let manifest = {
            let mut ctx = ParsingCtx::new(
                &source,
                &mut self.parse_state,
                &ast_data,
                &mut self.workspace,
                &mut self.interner,
            );
            ManifestAst::parse(&mut ctx)
        };

        let Ok(manifest) = manifest else {
            return (None, source);
        };

        *Rc::get_mut(&mut self.source).unwrap() = source;

        manifest.display(self);

        let source = mem::take(Rc::get_mut(&mut self.source).unwrap());
        (Some(&self.buffer), source)
    }

    pub fn imports(&mut self) {
        Rc::get_mut(&mut self.ast_data).unwrap().clear();
        let ast_data = self.ast_data.clone();
        let imports = {
            let mut ctx = ParsingCtx::new(
                &self.source,
                &mut self.parse_state,
                &ast_data,
                &mut self.workspace,
                &mut self.interner,
            );
            UseAst::parse(&mut ctx)
        };

        let Ok(imports) = imports else {
            return;
        };

        imports.display(self);
        self.newline();
        self.newline();
    }

    pub fn optional_newline(&mut self) {
        if !self.buffer.ends_with('\n') {
            self.newline();
        }
        self.write_indent();
    }

    pub fn write_span(&mut self, span: Span) {
        self.buffer.push_str(&self.source[span.range()]);
    }

    pub fn write_between(&mut self, fold: bool, range: Range<usize>) {
        let between = &self.source.clone()[range.clone()];
        let lexer = Lexer::new(between);
        lexer.write_between(self, fold, range.start);
    }

    pub fn list_element_len<T: FmtAst>(&self, element: &ListElement<T>) -> usize {
        element.value.flat_len(self)
            + self.folded_len_between(element.after_value.range())
            + element.after_delim.expand().map_or(0, |s| {
                self.folded_len_between(s.range()) + s.start() - element.after_value.end()
            })
    }

    pub fn folded_len_between(&self, range: Range<usize>) -> usize {
        Lexer::new(&self.source[range]).folded_len_between(self.line_length)
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn unindent(&mut self) {
        self.indent -= 1;
    }

    pub fn write_indent(&mut self) {
        self.buffer.extend(iter::repeat(' ').take(self.indent * 4));
    }

    pub fn newline(&mut self) {
        self.last_newline = self.buffer.len();
        self.buffer.push('\n');
    }

    pub fn line_progress(&self) -> usize {
        self.buffer.len() - self.last_newline
    }

    pub fn vis(&mut self, vis: scope::Vis) {
        match vis {
            scope::Vis::Pub => write!(self, "pub "),
            scope::Vis::None => (),
            scope::Vis::Priv => write!(self, "priv "),
        }
    }

    pub fn write_wrapped_span(&mut self, path: Span, wrapper: char) {
        self.buffer.push(wrapper);
        self.write_span(path);
        self.buffer.push(wrapper);
    }
}

// impl Fmt {
//     const LINE_LENGTH: usize = 80;

//     pub fn new() -> Self {
//         Self::default()
//     }

//     pub fn source(&mut self, source: String, path: Ident) -> (Option<&str>, String) {
//         self.clear(&source, path);

//         self.source = source;

//         if self.imports() {
//             return (None, mem::take(&mut self.source));
//         };

//         loop {
//             let ast_data = Rc::get_mut(&mut self.ast_data).expect("we are the only owner");

//             ast_data.clear();
//             let errors = self.workspace.error_count();
//             let (items, span, finished) = ParsingCtx::new(
//                 &self.source,
//                 &mut self.parse_state,
//                 ast_data,
//                 &mut self.workspace,
//                 &mut self.interner,
//             )
//             .parse_items_spanned();

//             if self.workspace.got_errors_since(errors) {
//                 break (None, mem::take(&mut self.source));
//             }

//             self.items(items, span);

//             if finished {
//                 break (Some(&self.buffer), mem::take(&mut self.source));
//             } else {
//                 write!(self, "break\n\n");
//             }
//         }
//     }

//     fn imports(&mut self) -> bool {
//         let error_count = self.workspace.error_count();

//         let ast_data = Rc::get_mut(&mut self.ast_data).expect("we are the only owner");
//         let imports = {
//             let mut ctx = ParsingCtx::new(
//                 &self.source,
//                 &mut self.parse_state,
//                 ast_data,
//                 &mut self.workspace,
//                 &mut self.interner,
//             );
//             UseAst::parse(&mut ctx).ok()
//         };

//         if let Some(imports) = imports {
//             self.imports_ast(imports);
//         }

//         self.workspace.got_errors_since(error_count)
//     }

//     fn imports_ast(&mut self, imports: UseAst) {
//         write!(self, "use ");
//         let imports_slice = &self.ast_data.clone()[imports.children];
//         self.list_low(
//             imports.span,
//             imports_slice,
//             "{",
//             "}",
//             "",
//             true,
//             true,
//             Self::import,
//         );
//     }

//     fn import(&mut self, ast: Ast) {
//         let [alias, path] = self.ast_data[ast.children] else {
//             unreachable!();
//         };

//         if alias.kind != AstKind::None {
//             self.ident(alias);
//             write!(self, " ");
//         }

//         self.ident(path);
//     }

//     fn clear(&mut self, source: &str, path: Ident) {
//         self.buffer.clear();
//         self.parse_state.start(source, path, false);
//         self.line_mapping = LineMapping::new(source);
//     }

//     pub fn into_workspace(self) -> Workspace {
//         self.workspace
//     }

//     fn items(&mut self, items: VSlice<Ast>, span: Span) {
//         let list = &self.ast_data.clone()[items];
//         self.list_low(span, list, "", "", "", true, false, Self::item);
//     }

//     fn item(&mut self, item: Ast) {
//         match item.kind {
//             AstKind::Struct { vis } => self.struct_decl(item, vis),

//             // AstKind::Bound { vis } => todo!(),
//             // AstKind::BoundType { vis } => todo!(),
//             // AstKind::BoundImpl { vis } => todo!(),
//             // AstKind::Impl { vis } => todo!(),
//             // AstKind::Func { vis } => todo!(),
//             kind => unimplemented!("{:?}", kind),
//         }
//     }

//     fn struct_decl(&mut self, item: Ast, vis: Vis) {
//         write!(self, "struct ");
//         self.vis(vis, false, true);

//         let [generics, name, body] = self.ast_data[item.children] else {
//             unreachable!()
//         };

//         self.generics(generics);

//         self.ident(name);

//         self.struct_body(body);
//     }

//     fn struct_body(&mut self, body: Ast) {
//         let fields = &self.ast_data.clone()[body.children];
//         self.list_low(
//             body.span,
//             fields,
//             " {",
//             "}",
//             "",
//             true,
//             true,
//             Self::struct_field,
//         );
//     }

//     fn struct_field(&mut self, field: Ast) {
//         let AstKind::StructField { vis, mutable, exported } = field.kind else {
//             unreachable!()
//         };

//         self.vis(vis, false, true);
//         self.exported(exported);
//         self.mutable(mutable);

//         let [name, ty] = self.ast_data[field.children] else {
//             unreachable!()
//         };

//         self.ident(name);
//         write!(self, ": ");
//         self.ty(ty);
//     }

//     fn generics(&mut self, generics: Ast) {
//         let list = &self.ast_data.clone()[generics.children];
//         if list.is_empty() {
//             return;
//         }
//         self.list(generics.span, list, "[", "] ", ",", |s, node| {
//             s.generic(node)
//         })
//     }

//     fn generic(&mut self, item: Ast) {
//         let [name, ref bounds @ ..] = self.ast_data.clone()[item.children] else {
//             unreachable!()
//         };

//         self.checkpoint();

//         self.ident(name);
//         let span = Span::new(name.span.end()..item.span.end());
//         self.switch(
//             span,
//             |s| s.list_low(span, bounds, ": ", "", " + ", false, false, Self::bound),
//             |s| s.list_low(span, bounds, ":", "", " +", true, true, Self::bound),
//         );
//     }

//     fn bound(&mut self, bound: Ast) {
//         match bound.kind {
//             AstKind::Ident | AstKind::IdentChain => self.ident(bound),
//             AstKind::TyInstance | AstKind::BoundInstance => self.bound_instance(bound),
//             kind => unimplemented!("{:?}", kind),
//         }
//     }

//     fn bound_instance(&mut self, bound: Ast) {
//         self.ty_instance(bound);
//     }

//     fn ty(&mut self, ty: Ast) {
//         match ty.kind {
//             AstKind::Ident | AstKind::IdentChain => self.ident(ty),
//             AstKind::TyInstance => self.ty_instance(ty),
//             AstKind::PointerTy => self.ty_pointer(ty),
//             AstKind::PointerMut => write!(self, "mut"),
//             kind => unimplemented!("{:?}", kind),
//         }
//     }

//     fn ty_pointer(&mut self, ty: Ast) {
//         let [mutability, base] = self.ast_data[ty.children] else {
//             unreachable!()
//         };

//         write!(self, "^");

//         if mutability.kind != AstKind::None {
//             self.ty(mutability);
//             write!(self, " ");
//         }

//         self.ty(base);
//     }

//     fn ty_instance(&mut self, ty: Ast) {
//         let [name, ref args @ ..] = self.ast_data.clone()[ty.children] else {
//             unreachable!()
//         };

//         self.ident(name);
//         self.list(
//             Span::new(name.span.end()..ty.span.end()),
//             args,
//             "[",
//             "]",
//             ",",
//             Self::ty,
//         );
//     }

//     fn ident(&mut self, item: Ast) {
//         write!(self, "{}", &self.source[item.span.range()]);
//     }

//     fn list(
//         &mut self,
//         span: Span,
//         ast: &[Ast],
//         start: &str,
//         end: &str,
//         sep: &str,
//         f: impl Fn(&mut Self, Ast) + Copy,
//     ) {
//         self.switch(
//             span,
//             |s| s.list_low(span, ast, start, end, sep, false, false, f),
//             |s| s.list_low(span, ast, start, end, sep, true, true, f),
//         )
//     }

//     fn list_low(
//         &mut self,
//         span: Span,
//         ast: &[Ast],
//         start: &str,
//         end: &str,
//         sep: &str,
//         wrap: bool,
//         indent: bool,
//         f: impl Fn(&mut Self, Ast),
//     ) {
//         let prev_wrap = self.wrap;
//         self.wrap = wrap;
//         let [first, ref rest @ ..] = ast else {
//             write!(self, "{}", start);
//             self.between(span.range());
//             write!(self, "{}", end);
//             return;
//         };

//         write!(self, "{}", start);
//         if wrap && indent {
//             self.indent();
//         }

//         if wrap && self.between(span.start()..first.span.start()) {
//             self.newline();
//             self.write_indent();
//         }

//         f(self, *first);

//         let mut prev = first.span;
//         for node in rest {
//             if wrap {
//                 write!(self, "{}", sep);
//                 if self.between(prev.end()..node.span.start()) {
//                     self.newline();
//                     self.write_indent();
//                 }
//             } else {
//                 write!(self, "{} ", sep);
//                 self.between(prev.end()..node.span.start());
//             }
//             f(self, *node);
//             prev = node.span;
//         }

//         if wrap {
//             write!(self, "{}", sep);
//             let should_newline = self.between(prev.end()..span.end());
//             if indent {
//                 self.unindent();
//             }
//             if should_newline {
//                 self.newline();
//                 self.write_indent();
//             } else {
//                 self.buffer.pop().unwrap();
//             }
//         }

//         write!(self, "{}", end);
//         self.wrap = prev_wrap;
//     }

//     fn switch(&mut self, region: Span, linear: impl Fn(&mut Self), wrapped: impl Fn(&mut Self)) {
//         self.checkpoint();
//         let prev = self.wrap;
//         let contains_line_comments =
//             Lexer::new(&self.source[region.range()]).contains_line_comments();
//         self.wrap = contains_line_comments;
//         if self.wrap {
//             wrapped(self);
//         } else {
//             linear(self);
//         }
//         self.wrap = prev;
//         if self.wrap && self.too_far() && !contains_line_comments {
//             self.revert();
//             self.wrap = true;
//             wrapped(self);
//             self.wrap = prev;
//         } else {
//             self.discard_checkpoint();
//         }
//     }

//     fn between(&mut self, range: Range<usize>) -> bool {
//         let str = &self.source[range];
//         let lexer = Lexer::new(str);
//         let prev_newline = self.last_newline;
//         lexer.translate(
//             &mut self.buffer,
//             &mut self.last_newline,
//             self.indent,
//             self.wrap,
//         );
//         self.last_newline == prev_newline
//     }

//     fn checkpoint(&mut self) {
//         self.checkpoints
//             .push((self.buffer.len(), self.last_newline));
//     }

//     fn discard_checkpoint(&mut self) {
//         self.checkpoints.pop().unwrap();
//     }

//     fn revert(&mut self) {
//         let (checkpoint, last_newline) = self.checkpoints.pop().unwrap();
//         self.buffer.truncate(checkpoint);
//         self.last_newline = last_newline;
//     }

//     fn too_far(&self) -> bool {
//         self.buffer.len() - self.last_newline > Self::LINE_LENGTH
//     }

//     fn vis(&mut self, vis: Vis, prev: bool, next: bool) {
//         if vis != Vis::None {
//             if prev {
//                 write!(self, " ");
//             }
//             write!(self, "{}", vis);
//             if next {
//                 write!(self, " ");
//             }
//         }
//     }

//     fn mutable(&mut self, mutable: bool) {
//         if mutable {
//             write!(self, "mut ");
//         }
//     }

//     fn exported(&mut self, exported: bool) {
//         if exported {
//             write!(self, "use ");
//         }
//     }

//     fn indent(&mut self) {
//         self.indent += 1;
//     }

//     fn unindent(&mut self) {
//         self.indent -= 1;
//     }

//     fn write_indent(&mut self) {
//         for _ in 0..self.indent {
//             self.buffer.push('\t');
//         }
//     }

//     fn newline(&mut self) {
//         self.last_newline = self.buffer.len();
//         self.buffer.push('\n');
//     }

//     // fn check_new_line(&mut self, str: &str) {
//     //     if str.contains("\n") {
//     //         self.last_newline = self.buffer.rfind('\n').unwrap() + 1;
//     //     };
//     // }
// }
