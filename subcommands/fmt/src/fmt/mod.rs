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
use packaging_t::Source;
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

impl<T: FmtAst> FmtAst for WrappedAst<T> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        fmt.write_span(self.start);

        if fold {
            fmt.newline();
            fmt.indent();
            fmt.write_indent();
        }

        self.value.display(fmt);

        if fold {
            fmt.newline();
            fmt.unindent();
            fmt.write_indent();
        }

        fmt.write_span(self.end);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.start.len() + self.end.len() + self.value.flat_len(fmt)
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

impl<'a> FmtAst for PathExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.start.display(fmt);
        for segment in self.segments.iter() {
            write!(fmt, "\\");
            segment.display(fmt);
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.segments
            .iter()
            .map(|ident| ident.flat_len(fmt))
            .intersperse(1)
            .sum::<usize>()
    }
}

impl<'a, T: FmtAst + Ast<'a> + Debug, META: ListAstMeta> FmtAst for ListAst<'a, T, META> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        let delimiter_spacing = META::START
            .iter()
            .any(|t| matches!(t, TokenPat::Kind(TokenKind::Colon | TokenKind::LeftCurly)));
        let is_top_list = META::END.contains(&TokenKind::Eof.into());
        if self.elements.is_empty() && META::OPTIONAL {
            return;
        }

        fmt.write_span(self.start);
        if fold && !is_top_list {
            fmt.indent();
        } else if !fold && delimiter_spacing {
            write!(fmt, " ");
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

            if let Some(after) = elem.after_delim {
                // unless we are folding, trailing separator is unwanted
                if !is_last || fold {
                    fmt.write_span(Span::new(elem.after_value.end()..after.start()));
                    if fmt.buffer.ends_with(['\n', ';'][fold as usize]) {
                        fmt.buffer.pop().unwrap();
                    }
                    if !fold {
                        fmt.buffer.push(' ');
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
        } else if is_top_list && !self.end.is_empty() {
            fmt.newline();
            fmt.newline();
        } else if !fold && delimiter_spacing && !self.end.is_empty() {
            write!(fmt, " ");
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

    fn clear(&mut self, source: &str) {
        self.buffer.clear();
        self.last_newline = 0;
        self.indent = 0;
        self.line_length = 80;
        self.interner.clear();
        self.parse_state.start(source);
        self.line_mapping = LineMapping::new(source);
    }

    pub fn source(&mut self, source_code: String, source: VRef<Source>) -> (Option<&str>, String) {
        self.clear(&source_code);

        *Rc::get_mut(&mut self.source).unwrap() = source_code;

        self.imports(source);

        loop {
            Rc::get_mut(&mut self.ast_data).unwrap().clear();
            let ast_data = self.ast_data.clone();

            let items = ParsingCtx::new(
                &self.source,
                &mut self.parse_state,
                &ast_data,
                &mut self.workspace,
                &mut self.interner,
                source,
            )
            .parse::<ItemsAst>();

            let Some(items) = items else {
                break (None, mem::take(Rc::get_mut(&mut self.source).unwrap()));
            };

            items.display_low(true, self);

            if items.end.is_empty() {
                break (
                    Some(&self.buffer),
                    mem::take(Rc::get_mut(&mut self.source).unwrap()),
                );
            }

            self.newline();
            self.newline();
        }
    }

    pub fn manifest(
        &mut self,
        source_code: String,
        source: VRef<Source>,
    ) -> (Option<&str>, String) {
        self.clear(&source_code);

        Rc::get_mut(&mut self.ast_data).unwrap().clear();
        let ast_data = self.ast_data.clone();
        let manifest = {
            let mut ctx = ParsingCtx::new(
                &source_code,
                &mut self.parse_state,
                &ast_data,
                &mut self.workspace,
                &mut self.interner,
                source,
            );
            ManifestAst::parse(&mut ctx)
        };

        let Some(manifest) = manifest else {
            return (None, source_code);
        };

        *Rc::get_mut(&mut self.source).unwrap() = source_code;

        manifest.display(self);

        let source = mem::take(Rc::get_mut(&mut self.source).unwrap());
        (Some(&self.buffer), source)
    }

    pub fn imports(&mut self, source: VRef<Source>) {
        Rc::get_mut(&mut self.ast_data).unwrap().clear();
        let ast_data = self.ast_data.clone();
        let imports = {
            let mut ctx = ParsingCtx::new(
                &self.source,
                &mut self.parse_state,
                &ast_data,
                &mut self.workspace,
                &mut self.interner,
                source,
            );
            UseAst::parse(&mut ctx)
        };

        let Some(imports) = imports else {
            return;
        };

        if imports.span() == Span::new(0..0) {
            return;
        }

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
            + element.after_delim.map_or(0, |s| {
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

    pub fn vis(&mut self, vis: Vis) {
        match vis {
            Vis::Pub => write!(self, "pub "),
            Vis::None => (),
            Vis::Priv => write!(self, "priv "),
        }
    }

    pub fn write_wrapped_span(&mut self, path: Span, wrapper: char) {
        self.buffer.push(wrapper);
        self.write_span(path);
        self.buffer.push(wrapper);
    }
}
