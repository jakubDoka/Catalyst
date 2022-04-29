use cranelift_entity::{packed_option::ReservedValue, *};
use lexer::*;

pub struct Data {
    pub nodes: PrimaryMap<Ast, Ent>,
    pub conns: StackMap<AstList, Ast>,
    pub elements: Vec<Ast>,
}

impl Data {
    pub fn new() -> Self {
        Self {
            nodes: PrimaryMap::new(),
            conns: StackMap::new(),
            elements: vec![],
        }
    }

    pub fn push(&mut self, node: Ast) {
        self.elements.push(node);
    }

    pub fn alloc(&mut self, kind: Kind, conns: &[Ast], span: Span) -> Ast {
        let list = self.conns.push(conns);
        let ent = Ent::new(kind, list, span);
        self.alloc_ent(ent)
    }

    pub fn alloc_sonless(&mut self, ident: Kind, span: Span) -> Ast {
        self.alloc_ent(Ent::childless(ident, span))
    }

    pub fn alloc_ent(&mut self, ent: Ent) -> Ast {
        self.nodes.push(ent)
    }

    #[inline]
    pub fn children(&self, ast: Ast) -> &[Ast] {
        self.conns.get(self.nodes[ast].children)
    }

    #[inline]
    pub fn elements(&self) -> impl Iterator<Item = (Ast, &Ent)> + Clone {
        self.elements
            .iter()
            .map(|&ast| (ast, &self.nodes[ast]))
    }

    #[inline]
    pub fn children_iter(&self, ast: Ast) -> impl Iterator<Item = &Ent> {
        self.children(ast)
            .iter()
            .map(|&ast| &self.nodes[ast])
    }

    pub fn slice(&self, list: AstList) -> &[Ast] {
        self.conns.get(list)
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
        self.conns.clear();
        self.elements.clear();
    }

    pub fn fmt(&self, f: &mut impl std::fmt::Write, source: Option<&str>) -> std::fmt::Result {
        for &ast in &self.elements {
            self.fmt_one(f, ast, 0, source)?;
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

        if children.is_reserved_value() {
            writeln!(f)?;
            return Ok(());
        }

        writeln!(f, ":")?;
        for child in self.conns.get(children) {
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
    pub children: AstList,
    pub span: Span,
}

impl Ent {
    pub fn new(kind: Kind, children: AstList, span: Span) -> Self {
        Ent {
            kind,
            children,
            span,
        }
    }

    pub fn childless(kind: Kind, span: Span) -> Self {
        Ent {
            kind,
            children: Default::default(),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    UseBoundFunc,
    Deref,
    Pointer,
    Char,
    Tag,
    Impl,
    ImplBody,
    GenericParam,
    Generics,
    Instantiation,
    Path,
    Bound,
    DotExpr,
    ConstructorField,
    ConstructorBody,
    Constructor,
    /// (used)
    SField(bool),
    StructBody,
    Struct,
    Break,
    Loop,
    /// (mutable)
    Variable(bool), 
    If,
    Unary,
    Binary,
    Index,
    Call,
    InlineConstructor,
    Return,
    Import,
    Imports,
    Block,
    /// (external)
    Function(bool),
    Instance,
    FunctionArgument,
    String,
    Ident,
    Int(i16),
    Bool(bool),
    None,
}

lexer::gen_entity!(Ast);
lexer::gen_entity!(AstList);
