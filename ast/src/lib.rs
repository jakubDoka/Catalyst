pub(self) use lexer::*;
use storage::*;

pub type AstTemp = FramedStack<Ast>;

pub const FUNCTION_ARG_START: usize = 3;
pub const FUNCTION_ARG_END: usize = 2;
pub const FUNCTION_RET: usize = 2;

pub fn id_of(ast: Ast, data: &AstData, sources: &Sources) -> ID {
    let AstEnt { span, .. } = data.nodes[ast];
    sources.id_of(span)
}

#[derive(Clone, Copy)]
pub struct AstEnt {
    pub kind: AstKind,
    pub children: AstList,
    pub span: Span,
}

impl AstEnt {
    pub fn new(kind: AstKind, children: AstList, span: Span) -> Self {
        AstEnt {
            kind,
            children,
            span,
        }
    }

    pub fn childless(kind: AstKind, span: Span) -> Self {
        AstEnt {
            kind,
            children: Default::default(),
            span,
        }
    }
}

pub struct AstData {
    pub nodes: PrimaryMap<Ast, AstEnt>,
    pub conns: StackMap<AstList, Ast>,
    pub elements: Vec<Ast>,
}

impl AstData {
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

    #[inline]
    pub fn alloc(&mut self, kind: AstKind, conns: &[Ast], span: Span) -> Ast {
        let list = self.conns.push(conns);
        let ent = AstEnt::new(kind, list, span);
        self.alloc_ent(ent)
    }

    #[inline]
    pub fn alloc_sonless(&mut self, ident: AstKind, span: Span) -> Ast {
        self.alloc_ent(AstEnt::childless(ident, span))
    }

    #[inline]
    pub fn alloc_ent(&mut self, ent: AstEnt) -> Ast {
        self.nodes.push(ent)
    }

    #[inline]
    pub fn children(&self, ast: Ast) -> &[Ast] {
        self.conns.get(self.nodes[ast].children)
    }

    #[inline]
    pub fn elements(&self) -> impl Iterator<Item = (Ast, &AstEnt)> + Clone {
        self.elements.iter().map(|&ast| (ast, &self.nodes[ast]))
    }

    #[inline]
    pub fn children_iter(&self, ast: Ast) -> impl Iterator<Item = &AstEnt> {
        self.children(ast).iter().map(|&ast| &self.nodes[ast])
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

        let AstEnt {
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

impl std::fmt::Debug for AstData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt(f, None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstKind {
    Continue,
    Label,
    FuncPtr,
    Ref(bool),
    TupleConstructorBody,
    StructPatternField,
    StructPattern,
    TupleStructBody,
    TupleStructPattern,
    Match,
    MatchArm,
    MatchBody,
    EnumVariant,
    EnumVariants,
    Enum,
    BitCast,
    Error,
    UseBoundFunc,
    Deref,
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
    Int,
    Bool,
    None,
}

gen_entity!(Ast);
gen_entity!(AstList);
