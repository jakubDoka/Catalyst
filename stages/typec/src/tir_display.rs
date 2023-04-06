use resources::{Resources, Source};
use type_creator::{type_creator, TypeCreator, TypeDisplay};

use {
    std::{
        fmt::{self, Write},
        iter,
    },
    storage::*,
    types::*,
};

pub struct TirDisplay<'ctx> {
    pub interner: &'ctx mut Interner,
    pub types: &'ctx mut Types,
    pub resources: &'ctx Resources,
}

impl<'ctx> TirDisplay<'ctx> {
    fn creator(&mut self) -> TypeCreator {
        type_creator!(self)
    }

    pub fn dbg_funcs(&mut self, funcs: &[(FragRef<Func>, TirFunc)]) {
        let mut buffer = String::new();
        self.display_funcs(funcs, &mut buffer).unwrap();
        print!("{buffer}");
    }

    pub fn display_funcs(
        &mut self,
        funcs: &[(FragRef<Func>, TirFunc)],
        buffer: &mut String,
    ) -> fmt::Result {
        for &(func, tir) in funcs.iter() {
            self.display_func(func, tir.body, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    fn display_func(
        &mut self,
        func: FragRef<Func>,
        tir: TirNode,
        buffer: &mut String,
    ) -> fmt::Result {
        self.types[func].display(self.types, self.interner, buffer)?;
        buffer.push(' ');
        let Func { signature, loc, .. } = self.types[func];
        self.display_tir(tir, buffer, 0, &mut signature.args.len(), loc.source())
    }

    pub fn display_tir(
        &mut self,
        TirNode { kind, ty, span, .. }: TirNode,
        buffer: &mut String,
        indent: usize,
        var_count: &mut usize,
        source: VRef<Source>,
    ) -> fmt::Result {
        match kind {
            TirKind::Int(computed) => {
                if let Some(computed) = computed {
                    write!(buffer, "{computed}")?;
                } else {
                    buffer.push_str(self.resources.span_str(source, span));
                }
            }
            TirKind::Float(computed) => {
                if let Some(computed) = computed {
                    write!(buffer, "{computed}")?;
                } else {
                    buffer.push_str(self.resources.span_str(source, span));
                }
            }
            TirKind::Char | TirKind::Bool(..) => {
                write!(buffer, "'{}'", self.resources.span_str(source, span))?;
            }
            TirKind::Block(nodes) => {
                let prev_var_count = *var_count;
                writeln!(buffer, "{{")?;
                let inner_ident = iter::repeat(' ').take((indent + 1) * 4);
                for &node in nodes {
                    buffer.extend(inner_ident.clone());
                    self.display_tir(node, buffer, indent + 1, var_count, source)?;
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
                    self.display_tir(val, buffer, indent, var_count, source)?;
                }
            }
            TirKind::Call(CallTir { func, params, args }) => {
                match *func {
                    CallableTir::Func(func) => {
                        write!(buffer, "{}", self.types[func].name.get(self.interner))?
                    }
                    CallableTir::SpecFunc(func) => {
                        let SpecFunc { parent, name, .. } = self.types[func];
                        write!(
                            buffer,
                            "{}\\{}",
                            self.creator().display_to_string(Spec::Base(parent)),
                            name.get(self.interner),
                        )?
                    }
                    CallableTir::Pointer(val) => {
                        self.display_tir(val, buffer, indent, var_count, source)?
                    }
                };

                if let Some((&first, others)) = params.split_first() {
                    write!(buffer, "[")?;
                    write!(buffer, "{}", self.creator().display_to_string(first))?;
                    for &other in others {
                        write!(buffer, ", {}", self.creator().display_to_string(other))?;
                    }
                    write!(buffer, "]")?;
                }

                write!(buffer, "(")?;
                if let Some((&first, others)) = args.split_first() {
                    self.display_tir(first, buffer, indent, var_count, source)?;
                    for &other in others {
                        write!(buffer, ", ")?;
                        self.display_tir(other, buffer, indent, var_count, source)?;
                    }
                }
                write!(buffer, ")")?;
            }
            TirKind::Access(var) => {
                write!(buffer, "var{}", var.index())?;
            }
            TirKind::Ctor(fields) => {
                write!(buffer, "{}\\{{", self.creator().display_to_string(ty))?;
                if let Some((&first, others)) = fields.split_first() {
                    self.display_tir(first, buffer, indent, var_count, source)?;
                    for &val in others.iter() {
                        write!(buffer, ", ")?;
                        self.display_tir(val, buffer, indent, var_count, source)?;
                    }
                }
                buffer.push('}');
            }
            TirKind::Deref(expr) => {
                write!(buffer, "*")?;
                self.display_tir(*expr, buffer, indent, var_count, source)?;
            }
            TirKind::Ref(expr) => {
                write!(buffer, "^")?;
                self.display_tir(*expr, buffer, indent, var_count, source)?;
            }
            TirKind::Match(&MatchTir { value, arms: cases }) => {
                write!(buffer, "match ")?;
                self.display_tir(value, buffer, indent, var_count, source)?;
                buffer.push_str(" {\n");
                let inner_ident = iter::repeat(' ').take((indent + 1) * 4);
                for MatchArmTir { pat, body } in cases {
                    buffer.extend(inner_ident.clone());
                    self.display_pat(*pat, buffer, indent + 1, var_count, source)?;
                    write!(buffer, " => ")?;
                    self.display_tir(*body, buffer, indent + 1, var_count, source)?;
                    buffer.push('\n');
                }
                buffer.extend(iter::repeat(' ').take(indent * 4));
                buffer.push('}');
            }
            TirKind::Field(&FieldTir { field, header }) => {
                self.display_tir(header, buffer, indent, var_count, source)?;
                write!(buffer, ".{field}")?;
            }
            TirKind::If(&IfTir { top, elifs, r#else }) => {
                write!(buffer, "if ")?;
                self.display_tir(top.cond, buffer, indent, var_count, source)?;
                write!(buffer, " ")?;
                self.display_tir(top.body, buffer, indent, var_count, source)?;
                for &IfBranchTir { cond, body } in elifs {
                    write!(buffer, " elif ")?;
                    self.display_tir(cond, buffer, indent, var_count, source)?;
                    write!(buffer, " ")?;
                    self.display_tir(body, buffer, indent, var_count, source)?;
                }
                if let Some(body) = r#else {
                    write!(buffer, " else ")?;
                    self.display_tir(body, buffer, indent, var_count, source)?;
                }
            }
            TirKind::Loop(&LoopTir { body, id }) => {
                write!(buffer, "loop id{} ", id.index())?;
                self.display_tir(body, buffer, indent, var_count, source)?;
            }
            TirKind::Break(&BreakTir { loop_id, value }) => {
                write!(buffer, "break id{} ", loop_id.index())?;
                if let Some(val) = value {
                    self.display_tir(val, buffer, indent, var_count, source)?;
                }
            }
            TirKind::Continue(loop_id) => {
                write!(buffer, "continue id{}", loop_id.index())?;
            }
            TirKind::Let(&LetTir { pat, value }) => {
                write!(buffer, "let ")?;
                self.display_pat(pat, buffer, indent, var_count, source)?;
                write!(buffer, " = ")?;
                self.display_tir(value, buffer, indent, var_count, source)?;
            }
            TirKind::Assign(&AssignTir { lhs, rhs }) => {
                self.display_tir(lhs, buffer, indent, var_count, source)?;
                write!(buffer, " = ")?;
                self.display_tir(rhs, buffer, indent, var_count, source)?;
            }
            TirKind::ConstAccess(r#const) => {
                let name = self.types[r#const].name.get(self.interner);
                buffer.push_str(name);
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
        source: VRef<Source>,
    ) -> fmt::Result {
        match pat.kind {
            PatKindTir::Unit(uint) => {
                self.display_unit_pat(uint, buffer, indent, var_count, source)
            }
            PatKindTir::Or(units) => {
                write!(buffer, "(")?;
                if let Some((&first, others)) = units.split_first() {
                    self.display_unit_pat(first, buffer, indent, var_count, source)?;
                    for &other in others {
                        write!(buffer, " | ")?;
                        self.display_unit_pat(other, buffer, indent, var_count, source)?;
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
        source: VRef<Source>,
    ) -> fmt::Result {
        match pat {
            UnitPatKindTir::Enum { id, value, .. } => {
                write!(buffer, "\\{id}~")?;
                if let Some(value) = value {
                    self.display_pat(*value, buffer, indent, var_count, source)?;
                }
            }
            UnitPatKindTir::Struct { fields } => {
                buffer.push_str("\\{");
                if let Some((&first, others)) = fields.split_first() {
                    self.display_pat(first, buffer, indent, var_count, source)?;
                    for &other in others {
                        write!(buffer, ", ")?;
                        self.display_pat(other, buffer, indent, var_count, source)?;
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
            UnitPatKindTir::Int(Ok(span), ..) => {
                buffer.push_str(self.resources.span_str(source, span))
            }
            UnitPatKindTir::Int(Err(lit), ..) => write!(buffer, "{lit}")?,
            UnitPatKindTir::Wildcard => buffer.push('_'),
        }
        Ok(())
    }
}
