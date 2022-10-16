use std::{
    fmt::{self, Write},
    iter,
};

use packaging_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl TyChecker<'_> {
    pub fn display_funcs(
        &self,
        funcs: &[(VRef<Func>, TirNode)],
        buffer: &mut String,
    ) -> fmt::Result {
        for &(func, tir) in funcs.iter() {
            self.display_func(func, tir, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    fn display_func(&self, func: VRef<Func>, tir: TirNode, buffer: &mut String) -> fmt::Result {
        self.typec.display_sig(func, self.interner, buffer)?;

        let Func { signature, .. } = self.typec.funcs[func];
        self.display_tir(tir, buffer, 0, &mut self.typec.args[signature.args].len())
    }

    pub fn display_tir(
        &self,
        TirNode { kind, ty, span }: TirNode,
        buffer: &mut String,
        indent: usize,
        var_count: &mut usize,
    ) -> fmt::Result {
        match kind {
            TirKind::Int(computed) => {
                if let Some(computed) = computed {
                    write!(buffer, "{}", computed)?;
                } else {
                    buffer.push_str(span_str!(self, span));
                }
            }
            TirKind::Char => {
                write!(buffer, "'{}'", span_str!(self, span))?;
            }
            TirKind::Block(nodes) => {
                let prev_var_count = *var_count;
                writeln!(buffer, "{{")?;
                let inner_ident = iter::repeat(' ').take((indent + 1) * 4);
                for &node in nodes {
                    buffer.extend(inner_ident.clone());
                    self.display_tir(node, buffer, indent + 1, var_count)?;
                    buffer.push('\n');
                }
                buffer.extend(iter::repeat(' ').take(indent * 4));
                buffer.push('}');
                *var_count = prev_var_count;
            }
            TirKind::Return(val) => {
                write!(buffer, "return")?;
                if let Some(&val) = val {
                    buffer.push(' ');
                    self.display_tir(val, buffer, indent, var_count)?;
                }
            }
            TirKind::Call(CallTir { func, params, args }) => {
                match *func {
                    CallableTir::Func(func) => {
                        write!(buffer, "{}", &self.interner[self.typec[func].name])?
                    }
                    CallableTir::SpecFunc(func) => {
                        let SpecFunc { parent, name, .. } = self.typec.spec_funcs[func];
                        write!(
                            buffer,
                            "{}\\{}",
                            &self.typec.display_spec(Spec::Base(parent), self.interner),
                            &self.interner[name],
                        )?
                    }
                    CallableTir::Pointer(val) => {
                        self.display_tir(val, buffer, indent, var_count)?
                    }
                };

                if let Some((&first, others)) = params.split_first() {
                    write!(buffer, "[")?;
                    write!(buffer, "{}", self.typec.display_ty(first, self.interner))?;
                    for &other in others {
                        write!(buffer, ", {}", self.typec.display_ty(other, self.interner))?;
                    }
                    write!(buffer, "]")?;
                }

                write!(buffer, "(")?;
                if let Some((&first, others)) = args.split_first() {
                    self.display_tir(first, buffer, indent, var_count)?;
                    for &other in others {
                        write!(buffer, ", ")?;
                        self.display_tir(other, buffer, indent, var_count)?;
                    }
                }
                write!(buffer, ")")?;
            }
            TirKind::Access(var) => {
                write!(buffer, "var{}", var.index())?;
            }
            TirKind::Const(value) => {
                write!(buffer, "const ")?;
                self.display_tir(*value, buffer, indent, var_count)?;
            }
            TirKind::Ctor(fields) => {
                write!(buffer, "{}\\{{", self.typec.display_ty(ty, self.interner))?;
                if let Some((&first, others)) = fields.split_first() {
                    self.display_tir(first, buffer, indent, var_count)?;
                    for &val in others.iter() {
                        write!(buffer, ", ")?;
                        self.display_tir(val, buffer, indent, var_count)?;
                    }
                }
                buffer.push('}');
            }
            TirKind::Deref(expr) => {
                write!(buffer, "*")?;
                self.display_tir(*expr, buffer, indent, var_count)?;
            }
            TirKind::Ref(expr) => {
                write!(buffer, "^")?;
                self.display_tir(*expr, buffer, indent, var_count)?;
            }
            TirKind::Match(&MatchTir { value, arms: cases }) => {
                write!(buffer, "match ")?;
                self.display_tir(value, buffer, indent, var_count)?;
                buffer.push_str(" {\n");
                let inner_ident = iter::repeat(' ').take((indent + 1) * 4);
                for MatchArmTir { pat, body } in cases {
                    buffer.extend(inner_ident.clone());
                    self.display_pat(*pat, buffer, indent + 1, var_count)?;
                    write!(buffer, " => ")?;
                    self.display_tir(*body, buffer, indent + 1, var_count)?;
                    buffer.push('\n');
                }
                buffer.extend(iter::repeat(' ').take(indent * 4));
                buffer.push('}');
            }
            TirKind::Field(&FieldTir { field, header }) => {
                self.display_tir(header, buffer, indent, var_count)?;
                write!(buffer, ".{}", field)?;
            }
            TirKind::If(&IfTir { top, elifs, r#else }) => {
                write!(buffer, "if ")?;
                self.display_tir(top.cond, buffer, indent, var_count)?;
                write!(buffer, " ")?;
                self.display_tir(top.body, buffer, indent, var_count)?;
                for &IfBranchTir { cond, body } in elifs {
                    write!(buffer, " elif ")?;
                    self.display_tir(cond, buffer, indent, var_count)?;
                    write!(buffer, " ")?;
                    self.display_tir(body, buffer, indent, var_count)?;
                }
                if let Some(body) = r#else {
                    write!(buffer, " else ")?;
                    self.display_tir(body, buffer, indent, var_count)?;
                }
            }
            TirKind::Let(&LetTir { pat, value }) => {
                write!(buffer, "let ")?;
                self.display_pat(pat, buffer, indent, var_count)?;
                write!(buffer, " = ")?;
                self.display_tir(value, buffer, indent, var_count)?;
            }
            TirKind::Assign(&AssignTir { lhs, rhs }) => {
                self.display_tir(lhs, buffer, indent, var_count)?;
                write!(buffer, " = ")?;
                self.display_tir(rhs, buffer, indent, var_count)?;
            }
        }

        Ok(())
    }

    fn display_pat(
        &self,
        pat: PatTir,
        buffer: &mut String,
        indent: usize,
        var_count: &mut usize,
    ) -> fmt::Result {
        match pat.kind {
            PatKindTir::Unit(uint) => self.display_unit_pat(uint, buffer, indent, var_count),
            PatKindTir::Or(units) => {
                write!(buffer, "(")?;
                if let Some((&first, others)) = units.split_first() {
                    self.display_unit_pat(first, buffer, indent, var_count)?;
                    for &other in others {
                        write!(buffer, " | ")?;
                        self.display_unit_pat(other, buffer, indent, var_count)?;
                    }
                }
                write!(buffer, ")")?;
                Ok(())
            }
        }
    }

    fn display_unit_pat(
        &self,
        pat: UnitPatKindTir,
        buffer: &mut String,
        indent: usize,
        var_count: &mut usize,
    ) -> fmt::Result {
        match pat {
            UnitPatKindTir::Struct { fields } => {
                buffer.push_str("\\{");
                if let Some((&first, others)) = fields.split_first() {
                    self.display_pat(first, buffer, indent, var_count)?;
                    for &other in others {
                        write!(buffer, ", ")?;
                        self.display_pat(other, buffer, indent, var_count)?;
                    }
                }
                buffer.push('}');
            }
            UnitPatKindTir::Binding(mutable, var) => {
                if mutable {
                    buffer.push_str("mut ");
                }
                write!(buffer, "var{}", var.index())?;
                *var_count = var.index() + 1;
            }
            UnitPatKindTir::Int(Ok(span), ..) => buffer.push_str(span_str!(self, span)),
            UnitPatKindTir::Int(Err(lit), ..) => write!(buffer, "{}", lit)?,
            UnitPatKindTir::Wildcard => buffer.push('_'),
        }
        Ok(())
    }
}
