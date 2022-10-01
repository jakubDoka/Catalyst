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
        self.display_tir(
            tir,
            buffer,
            0,
            &mut self.typec.ty_slices[signature.args].len(),
        )
    }

    pub fn display_tir(
        &self,
        tir: TirNode,
        buffer: &mut String,
        indent: usize,
        var_count: &mut usize,
    ) -> fmt::Result {
        match tir {
            TirNode::Var(Variable { value, .. }) => {
                write!(buffer, "let var{} = ", *var_count)?;
                *var_count += 1;
                self.display_tir(value.unwrap(), buffer, indent, var_count)?;
            }
            TirNode::Int(int) => {
                buffer.push_str(span_str!(self, int.span));
            }
            TirNode::Char(span) => {
                write!(buffer, "'{}'", span_str!(self, span))?;
            }
            TirNode::Block(BlockTir { nodes, .. }) => {
                let prev_var_count = *var_count;
                writeln!(buffer, "{{")?;
                let inner_ident = iter::repeat(' ').take((indent + 1) * 4);
                for &node in *nodes {
                    buffer.extend(inner_ident.clone());
                    self.display_tir(node, buffer, indent + 1, var_count)?;
                    buffer.push('\n');
                }
                buffer.extend(iter::repeat(' ').take(indent * 4));
                write!(buffer, "}}")?;
                *var_count = prev_var_count;
            }
            TirNode::Return(ReturnTir { val, .. }) => {
                write!(buffer, "return")?;
                if let &Some(val) = val {
                    buffer.push(' ');
                    self.display_tir(val, buffer, indent, var_count)?;
                }
            }
            TirNode::Call(CallTir {
                func, params, args, ..
            }) => {
                match *func {
                    CallableTir::Func(func) => {
                        write!(buffer, "{}", &self.interner[self.typec.funcs.id(func)])?
                    }
                    CallableTir::BoundFunc(_) => todo!(),
                    CallableTir::Pointer(val) => {
                        self.display_tir(val, buffer, indent, var_count)?
                    }
                };

                if let Some((&first, others)) = params.split_first() {
                    write!(buffer, "[")?;
                    write!(buffer, "{}", &self.interner[self.typec.types.id(first)])?;
                    for &other in others {
                        write!(buffer, ", {}", &self.interner[self.typec.types.id(other)])?;
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
            TirNode::Access(AccessTir { var, .. }) => {
                write!(buffer, "var{}", var.index())?;
            }
            TirNode::Const(ConstTir { value, .. }) => {
                write!(buffer, "const ")?;
                self.display_tir(*value, buffer, indent, var_count)?;
            }
        }

        Ok(())
    }
}
