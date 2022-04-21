use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityList, EntitySet, ListPool, PrimaryMap,
};

use crate::*;
use lexer::*;

pub struct Temp {
    pub frames: Vec<usize>,
    pub stack: Vec<Tir>,
}

impl Temp {
    pub fn new() -> Self {
        Self {
            frames: vec![Default::default()],
            stack: Vec::new(),
        }
    }

    pub fn frame_view(&self) -> &[Tir] {
        let &start = self.frames.last().unwrap();
        &self.stack[start..]
    }

    pub fn acc(&mut self, tir: Tir) {
        self.stack.push(tir);
    }

    pub fn mark_frame(&mut self) {
        let len = self.stack.len();
        self.frames.push(len);
    }

    pub fn pop_frame(&mut self) {
        let frame = self.frames.pop().unwrap();
        self.stack.truncate(frame);
    }
}

lexer::gen_entity!(FrameId);

#[derive(Clone)]
pub struct Data {
    pub ents: PrimaryMap<Tir, Ent>,
    pub cons: ListPool<Tir>,
}

impl Data {
    pub fn new() -> Self {
        Self {
            ents: PrimaryMap::new(),
            cons: ListPool::new(),
        }
    }
}

impl Default for Data {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Ent {
    pub kind: Kind,
    pub ty: Ty,
    pub flags: Flags,
    pub span: Span,
}

impl Ent {
    pub fn with_flags(kind: Kind, ty: Ty, flags: Flags, span: Span) -> Self {
        Self {
            kind,
            ty,
            flags,
            span,
        }
    }

    pub fn new(kind: Kind, ty: Ty, span: Span) -> Self {
        Self::with_flags(kind, ty, Default::default(), span)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    Variable(Tir),
    Access(Tir),
    Assign(Tir, Tir),
    Break(Tir, PackedOption<Tir>),
    Loop(Tir),
    LoopInProgress(PackedOption<Tir>, bool),
    FieldAccess(Tir, ID),
    Constructor(EntityList<Tir>),
    If(Tir, Tir, PackedOption<Tir>),
    Block(EntityList<Tir>),
    Return(PackedOption<Tir>),
    Argument(u32),
    Call(Func, EntityList<Tir>),
    IntLit(i16),
    BoolLit(bool),
    Invalid,
}

impl Kind {
    pub fn is_terminating(&self) -> bool {
        matches!(self, Kind::Return(_))
    }
}

impl Default for Kind {
    fn default() -> Self {
        Self::Invalid
    }
}

lexer::gen_entity!(Tir);

bitflags::bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        /// Can we assign to this expression?
        const ASSIGNABLE = 1 << 2;
        /// This expression terminates execution.
        const TERMINATING = 1 << 3;
    }
}

#[macro_export]
macro_rules! impl_bool_bit_and {
    ($flags:ty) => {
        impl std::ops::BitAnd<bool> for $flags {
            type Output = Self;
        
            fn bitand(self, rhs: bool) -> Self {
                Self { bits: self.bits & (!rhs as u32).wrapping_add(u32::MAX) }
            }
        }        
    };
}

impl_bool_bit_and!(Flags);

pub struct Display<'a> {
    pub types: &'a Types,
    pub sources: &'a Sources,
    pub data: &'a Data,
    pub root: Tir,
}

impl<'a> Display<'a> {
    pub fn new(types: &'a Types, sources: &'a Sources, data: &'a Data, root: Tir) -> Self {
        Self {
            types,
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
                ty::Display::new(self.types, self.sources, self.data.ents[root].ty)
            )?;
        }
        displayed.insert(root);

        let ent = self.data.ents[root];
        match ent.kind {
            Kind::FieldAccess(expr, id) => {
                self.fmt(expr, f, displayed, level, false)?;
                write!(f, ".{}", self.types.fields.get(id).unwrap().index)?;
            }
            Kind::Constructor(fields) => {
                writeln!(f, "::{{")?;
                for &field in self.data.cons.view(fields).iter() {
                    self.fmt(field, f, displayed, level + 1, true)?;
                }
                for _ in 0..level {
                    write!(f, "  ")?;
                }
                write!(f, "}}")?;
            }
            Kind::If(cond, then, otherwise) => {
                write!(f, "if ")?;
                self.fmt(cond, f, displayed, level, false)?;
                write!(f, " then ")?;
                self.fmt(then, f, displayed, level, false)?;
                if let Some(otherwise) = otherwise.expand() {
                    write!(f, " else ")?;
                    self.fmt(otherwise, f, displayed, level, false)?;
                }
            }
            Kind::Block(content) => {
                if !content.is_empty() {
                    writeln!(f, "{{")?;
                    for &expr in self.data.cons.view(content).iter() {
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
            Kind::Return(expr) => {
                write!(f, "return ")?;
                if let Some(expr) = expr.expand() {
                    self.fmt(expr, f, displayed, level, false)?;
                }
            }
            Kind::Argument(id) => {
                write!(f, "parameter {}", id)?;
            }
            Kind::Call(func, args) => {
                write!(f, "{func}(")?;
                for (i, &arg) in self.data.cons.view(args).iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.fmt(arg, f, displayed, level, false)?;
                }

                write!(f, ")")?;
            }
            Kind::IntLit(_) => {
                write!(f, "{}", self.sources.display(ent.span))?;
            }
            Kind::BoolLit(_) => {
                write!(f, "{}", self.sources.display(ent.span))?;
            }
            Kind::Loop(block) => {
                write!(f, "loop ")?;
                self.fmt(block, f, displayed, level, false)?;
            }
            Kind::Break(loop_expr, ret) => {
                write!(f, "break ")?;
                self.fmt(loop_expr, f, displayed, level, false)?;
                if let Some(ret) = ret.expand() {
                    write!(f, " ")?;
                    self.fmt(ret, f, displayed, level, false)?;
                }
            }
            Kind::Assign(left, right) => {
                self.fmt(left, f, displayed, level, false)?;
                write!(f, " = ")?;
                self.fmt(right, f, displayed, level, false)?;
            }
            Kind::Variable(tir) => {
                write!(f, "let ")?;
                self.fmt(tir, f, displayed, level, false)?;
            }
            Kind::Access(value) => {
                self.fmt(value, f, displayed, level, false)?;
            }
            Kind::Invalid | Kind::LoopInProgress(..) => unreachable!(),
        }

        if ident {
            writeln!(f)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut displayed = EntitySet::new();
        self.fmt(self.root, f, &mut displayed, 1, true)
    }
}
