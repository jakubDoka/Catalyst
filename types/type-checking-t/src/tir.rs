use crate::*;
use lexing_t::*;
use storage::*;

pub type TirData = BumpMap<TirList, TirEnt, Tir>;

impl TirDisplay<'_> {
    pub fn display(&self, to: &mut dyn std::fmt::Write) -> std::fmt::Result {
        writeln!(to, "fn {} {{", self.name)?;

        for &node in &self.tir_data[self.root] {
            write!(to, "\t")?;
            self.display_recur(node, 1, to)?;
            writeln!(to)?;
        }

        writeln!(to, "}}")
    }

    fn display_recur(
        &self,
        node: TirEnt,
        depth: usize,
        to: &mut dyn std::fmt::Write,
    ) -> std::fmt::Result {
        let indent = "\t".repeat(depth);
        match node.kind {
            TirKind::Block { stmts } => {
                writeln!(to, "{{")?;
                for &stmt in &self.tir_data[stmts] {
                    write!(to, "{}", indent)?;
                    self.display_recur(stmt, depth + 1, to)?;
                    writeln!(to)?;
                }
                writeln!(to, "}}")?;
            }
            TirKind::Return { value } => {
                write!(to, "return")?;
                if let Some(value) = value.expand() {
                    write!(to, " ")?;
                    self.display_recur(self.tir_data[value], depth, to)?;
                }
            }
            TirKind::Access { var } => {
                write!(to, "access {var}")?;
            }
            TirKind::String | TirKind::Int => {
                write!(
                    to,
                    "{}",
                    self.packages.span_str(self.ident, node.span.unwrap())
                )?;
            }
            TirKind::Argument(i) => {
                write!(to, "argument {i}")?;
            }
            TirKind::Call { def, params, args } => {
                write!(to, "call ")?;

                let name = &self.interner[self.typec.defs.id(def)];
                write!(to, "{}", name)?;

                if let Some((&first, others)) = self.typec.ty_lists[params].split_first() {
                    write!(to, "[{}", &self.interner[self.typec.types.id(first)])?;
                    for &param in others {
                        write!(to, ", {}", &self.interner[self.typec.types.id(param)])?;
                    }
                    write!(to, "]")?;
                }

                if let Some((&first, others)) = self.tir_data[args].split_first() {
                    write!(to, "(")?;
                    self.display_recur(first, depth, to)?;
                    for &arg in others {
                        write!(to, ", ")?;
                        self.display_recur(arg, depth, to)?;
                    }
                    write!(to, ")")?;
                }
            }
            TirKind::Unreachable => write!(to, "unreachable")?,
            TirKind::Invalid => write!(to, "invalid")?,
        }

        Ok(())
    }
}

#[derive(Default, Clone, Copy)]
pub struct TirEnt {
    pub kind: TirKind,
    pub flags: TirFlags,
    pub ty: Maybe<Ty>,
    pub span: Maybe<Span>,
}

impl TirEnt {
    pub fn terminating(&self) -> bool {
        self.flags.contains(TirFlags::TERMINATING)
    }

    #[inline]
    pub fn new(kind: TirKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    #[inline]
    pub fn ty(self, ty: impl Into<Maybe<Ty>>) -> Self {
        Self {
            ty: ty.into(),
            ..self
        }
    }

    #[inline]
    pub fn span(self, span: impl Into<Maybe<Span>>) -> Self {
        Self {
            span: span.into(),
            ..self
        }
    }

    #[inline]
    pub fn flags(self, flags: TirFlags) -> Self {
        Self { flags, ..self }
    }
}

#[derive(Default, Clone, Copy)]
pub struct TirMeta {}

#[derive(Default, Clone, Copy)]
pub enum TirKind {
    Block {
        stmts: Maybe<TirList>,
    },
    Return {
        value: Maybe<Tir>,
    },
    Access {
        var: Tir,
    },
    Call {
        def: Def,
        params: Maybe<TyList>,
        args: Maybe<TirList>,
    },
    String,
    Int,
    Argument(u8),
    Unreachable,
    #[default]
    Invalid,
}

bitflags! {
    struct TirFlags: u8 {
        TERMINATING
    }
}

gen_v_ptr!(Tir TirList);
