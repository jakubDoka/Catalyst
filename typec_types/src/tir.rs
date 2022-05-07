use storage::*;
use lexer_types::*;
use crate::*;

#[derive(Clone)]
pub struct TirData {
    pub ents: PrimaryMap<Tir, TirEnt>,
    pub cons: StackMap<TirList, Tir>,
    pub used_types: TyList,
}

impl TirData {
    pub fn new() -> Self {
        Self {
            ents: PrimaryMap::new(),
            cons: StackMap::new(),
            used_types: TyList::reserved_value(),
        }
    }
}

impl Default for TirData {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TirEnt {
    pub kind: TirKind,
    pub ty: Ty,
    pub flags: TirFlags,
    pub span: Span,
}

impl TirEnt {
    pub fn with_flags(kind: TirKind, ty: Ty, flags: TirFlags, span: Span) -> Self {
        Self {
            kind,
            ty,
            flags,
            span,
        }
    }

    pub fn new(kind: TirKind, ty: Ty, span: Span) -> Self {
        Self::with_flags(kind, ty, Default::default(), span)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TirKind {
    Type,
    DerefPointer(Tir),
    TakePtr(Tir),
    Variable(Tir),
    Access(Tir),
    Assign(Tir, Tir),
    Break(Tir, PackedOption<Tir>),
    Loop(Tir),
    LoopInProgress(PackedOption<Tir>, bool),
    FieldAccess(Tir, SField),
    Constructor(TirList),
    If(Tir, Tir, PackedOption<Tir>),
    Block(TirList),
    Return(PackedOption<Tir>),
    Argument(u32),
    Call(PackedOption<Ty>, Func, TirList),
    IntLit(i16),
    BoolLit(bool),
    CharLit,
    Invalid,
}

impl TirKind {
    pub fn is_terminating(&self) -> bool {
        matches!(self, TirKind::Return(_))
    }
}

impl Default for TirKind {
    fn default() -> Self {
        Self::Invalid
    }
}

gen_entity!(Tir);
gen_entity!(TirList);

bitflags! {
    #[derive(Default)]
    pub struct TirFlags: u32 {
        /// Can we assign to this expression?
        const ASSIGNABLE = 1 << 0;
        /// This expression terminates execution.
        const TERMINATING = 1 << 1;
        /// This expression will always reside stack
        const SPILLED = 1 << 2;
    }
}

impl_bool_bit_and!(TirFlags);

pub struct TirDisplay<'a> {
    pub types: &'a Types,
    pub ty_lists: &'a TyLists,
    pub sfields: &'a SFields,
    pub sources: &'a Sources,
    pub data: &'a TirData,
    pub root: Tir,
}

impl<'a> TirDisplay<'a> {

    #[inline(never)]
    pub fn new(types: &'a Types, ty_lists: &'a TyLists, sfields: &'a SFields, sources: &'a Sources, data: &'a TirData, root: Tir) -> Self {
        Self {
            types,
            ty_lists,
            sfields,
            sources,
            data,
            root,
        }
    }

    fn fmt(
        &self,
        root: Tir,
        f: &mut std::fmt::Formatter<'_>,
        displayed: &mut EntitySet<Tir>,
        level: usize,
        ident: bool,
    ) -> std::fmt::Result {
        if root.is_reserved_value() {
            return write!(f, "!!!reserved value encountered!!!");
        }
        if ident {
            for _ in 0..level {
                write!(f, "  ")?;
            }
        }
        if displayed.contains(root) {
            write!(f, "{root}")?;
            if ident {
                writeln!(f)?;
            }
            return Ok(());
        } else {
            write!(
                f,
                "{root}: {} = ",
                ty_display!(self, self.data.ents[root].ty)
            )?;
        }
        displayed.insert(root);

        let ent = self.data.ents[root];
        match ent.kind {
            TirKind::TakePtr(tir) => {
                write!(f, "take_pointer ")?;
                self.fmt(tir, f, displayed, level + 1, true)?;
            }
            TirKind::DerefPointer(tir) => {
                write!(f, "deref_pointer ")?;
                self.fmt(tir, f, displayed, level + 1, true)?;
            }
            TirKind::FieldAccess(expr, id) => {
                self.fmt(expr, f, displayed, level, false)?;
                write!(f, ".{}", self.sfields[id].index)?;
            }
            TirKind::Constructor(fields) => {
                writeln!(f, "::{{")?;
                for &field in self.data.cons.get(fields).iter() {
                    self.fmt(field, f, displayed, level + 1, true)?;
                }
                for _ in 0..level {
                    write!(f, "  ")?;
                }
                write!(f, "}}")?;
            }
            TirKind::If(cond, then, otherwise) => {
                write!(f, "if ")?;
                self.fmt(cond, f, displayed, level, false)?;
                write!(f, " then ")?;
                self.fmt(then, f, displayed, level, false)?;
                if let Some(otherwise) = otherwise.expand() {
                    write!(f, " else ")?;
                    self.fmt(otherwise, f, displayed, level, false)?;
                }
            }
            TirKind::Block(content) => {
                if !content.is_reserved_value() {
                    writeln!(f, "{{")?;
                    for &expr in self.data.cons.get(content).iter() {
                        self.fmt(expr, f, displayed, level + 1, true)?;
                    }
                    for _ in 0..level {
                        write!(f, "  ")?;
                    }
                    write!(f, "}}")?;
                } else {
                    write!(f, "{{}}")?;
                }
            }
            TirKind::Return(expr) => {
                write!(f, "return ")?;
                if let Some(expr) = expr.expand() {
                    self.fmt(expr, f, displayed, level, false)?;
                }
            }
            TirKind::Argument(id) => {
                write!(f, "parameter {}", id)?;
            }
            TirKind::Type => {
                write!(f, "<type>")?;
            }
            TirKind::Call(caller, func, args) => {
                if let Some(caller) = caller.expand() {
                    write!(
                        f,
                        "{}::",
                        ty_display!(self, caller)
                    )?;
                }
                write!(f, "{func}(")?;
                for (i, &arg) in self.data.cons.get(args).iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.fmt(arg, f, displayed, level, false)?;
                }

                write!(f, ")")?;
            }
            TirKind::IntLit(..) | TirKind::CharLit | TirKind::BoolLit(_) => {
                write!(f, "{}", self.sources.display(ent.span))?;
            }
            TirKind::Loop(block) => {
                write!(f, "loop ")?;
                self.fmt(block, f, displayed, level, false)?;
            }
            TirKind::Break(loop_expr, ret) => {
                write!(f, "break ")?;
                self.fmt(loop_expr, f, displayed, level, false)?;
                if let Some(ret) = ret.expand() {
                    write!(f, " ")?;
                    self.fmt(ret, f, displayed, level, false)?;
                }
            }
            TirKind::Assign(left, right) => {
                self.fmt(left, f, displayed, level, false)?;
                write!(f, " = ")?;
                self.fmt(right, f, displayed, level, false)?;
            }
            TirKind::Variable(tir) => {
                write!(f, "let ")?;
                self.fmt(tir, f, displayed, level, false)?;
            }
            TirKind::Access(value) => {
                self.fmt(value, f, displayed, level, false)?;
            }
            TirKind::Invalid | TirKind::LoopInProgress(..) => unreachable!(),
        }

        if ident {
            writeln!(f)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for TirDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut displayed = EntitySet::new();
        self.fmt(self.root, f, &mut displayed, 1, true)
    }
}