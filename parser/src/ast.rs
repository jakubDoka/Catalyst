pub use crate::error::Error;
use cranelift_entity::{packed_option::ReservedValue, *};
use lexer::*;

#[derive(Debug)]
pub struct Temp {
    frames: Vec<usize>,
    conn: Vec<Ast>,
}

impl Temp {
    pub fn new() -> Self {
        Temp {
            frames: Vec::new(),
            conn: Vec::new(),
        }
    }

    pub fn bottom(&mut self, ast: Ast) {
        self.mark_frame();
        self.acc(ast);
    }

    pub fn save_frame(&mut self, file: &mut Data) -> EntityList<Ast> {
        let frame = file.alloc_conn(&self.conn[*self.frames.last().unwrap()..]);
        self.pop_frame();
        frame
    }

    pub fn frame_view(&self) -> &[Ast] {
        &self.conn[*self.frames.last().unwrap_or(&0)..]
    }

    pub fn acc(&mut self, ast: Ast) {
        self.conn.push(ast)
    }

    pub fn clear_conn(&mut self) {
        self.conn.clear()
    }

    pub fn acc_nil(&mut self) {
        self.acc(Ast::reserved_value());
    }

    pub fn mark_frame(&mut self) {
        self.frames.push(self.conn.len());
    }

    pub fn pop_frame(&mut self) {
        let len = self.frames.pop().unwrap();
        self.conn.truncate(len);
    }
}

pub struct Data {
    pub nodes: PrimaryMap<Ast, Ent>,
    pub conns: ListPool<Ast>,
    pub elements: EntityList<Ast>,
}

impl Data {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            conns: ListPool::new(),
            elements: EntityList::new(),
        }
    }

    pub fn push(&mut self, node: Ast) {
        self.elements.push(node, &mut self.conns);
    }

    pub fn alloc(&mut self, kind: Kind, conns: &[Ast], span: Span) -> Ast {
        let list = self.alloc_conn(conns);
        let ent = Ent::new(kind, list, span);
        self.alloc_ent(ent)
    }

    pub fn alloc_sonless(&mut self, ident: Kind, span: Span) -> Ast {
        self.alloc_ent(Ent::childless(ident, span))
    }

    pub fn alloc_ent(&mut self, ent: Ent) -> Ast {
        self.nodes.push(ent)
    }

    pub fn alloc_conn(&mut self, vec: &[Ast]) -> EntityList<Ast> {
        EntityList::from_slice(vec, &mut self.conns)
    }

    #[inline]
    pub fn children(&self, ast: Ast) -> &[Ast] {
        self.nodes[ast].children.as_slice(&self.conns)
    }

    #[inline]
    pub fn elements(&self) -> impl Iterator<Item = (Ast, &Ent)> {
        self.elements
            .as_slice(&self.conns)
            .iter()
            .map(|&ast| (ast, &self.nodes[ast]))
    }

    #[inline]
    pub fn children_iter(&self, ast: Ast) -> impl Iterator<Item = &Ent> {
        self.nodes[ast]
            .children
            .as_slice(&self.conns)
            .iter()
            .map(|&ast| &self.nodes[ast])
    }

    pub fn slice(&self, list: EntityList<Ast>) -> &[Ast] {
        list.as_slice(&self.conns)
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
        self.conns.clear();
        self.elements = EntityList::new();
    }

    pub fn fmt(&self, f: &mut impl std::fmt::Write, source: Option<&str>) -> std::fmt::Result {
        for ast in self.elements.as_slice(&self.conns) {
            self.fmt_one(f, *ast, 0, source)?;
        }
        Ok(())
    }

    fn fmt_one(
        &self,
        f: &mut impl std::fmt::Write,
        ast: Ast,
        depth: usize,
        source: Option<&str>,
    ) -> std::fmt::Result {
        for _ in 0..depth {
            write!(f, " ")?;
        }

        if ast.is_reserved_value() {
            writeln!(f, "NIL")?;
            return Ok(());
        }

        let Ent {
            kind,
            children,
            span,
        } = self.nodes[ast];
        write!(f, "{:?}", kind)?;

        if let Some(source) = source {
            write!(f, " {:?}", &source[span.range()])?;
        }

        if children.is_empty() {
            writeln!(f)?;
            return Ok(());
        }

        writeln!(f, ":")?;
        for child in children.as_slice(&self.conns) {
            self.fmt_one(f, *child, depth + 1, source)?;
        }

        Ok(())
    }
}

impl std::fmt::Debug for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt(f, None)
    }
}

pub struct FileDisplay<'a> {
    file: &'a Data,
    source: &'a str,
}

impl<'a> FileDisplay<'a> {
    pub fn new(file: &'a Data, source: &'a str) -> Self {
        Self { file, source }
    }
}

impl std::fmt::Display for FileDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.file.fmt(f, Some(self.source))
    }
}

#[derive(Clone, Copy)]
pub struct Ent {
    pub kind: Kind,
    pub children: EntityList<Ast>,
    pub span: Span,
}

impl Ent {
    pub fn new(kind: Kind, children: EntityList<Ast>, span: Span) -> Self {
        Ent {
            kind,
            children,
            span,
        }
    }

    pub fn childless(kind: Kind, span: Span) -> Self {
        Ent {
            kind,
            children: EntityList::new(),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    If,
    Binary,
    Index,
    Call,
    StructField,
    InlineConstructor,
    Return,
    Import,
    Imports,
    Block,
    Function,
    Instance,
    FunctionArgument,
    String,
    Ident,
    Int(i16),
    Bool(bool),
    None,
}

lexer::gen_entity!(Ast);
