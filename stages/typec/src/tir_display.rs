use std::{
    default::default,
    fmt::{self, Write},
    iter,
};

use packaging_t::span_str;
use storage::VRef;
use typec_t::{AccessTir, BlockTir, Func, ReturnTir, TirNode, TypeCheckedFuncs, Variable};

use crate::*;

impl TyChecker<'_> {
    pub fn display_funcs(&self, funcs: &TypeCheckedFuncs, buffer: &mut String) -> fmt::Result {
        for &(func, tir) in funcs.iter() {
            self.display_func(func, tir, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    fn display_func(
        &self,
        func: VRef<Func>,
        tir: Option<TirNode>,
        buffer: &mut String,
    ) -> fmt::Result {
        let Func { signature, .. } = self.typec.funcs[func];
        write!(
            buffer,
            "fn {}[todo] {}({}) -> {} ",
            signature
                .cc
                .expand()
                .map(|cc| &self.interner[cc])
                .map_or(default(), |cc| format!("\"{}\" ", cc)),
            &self.interner[self.typec.funcs.id(func)],
            self.typec.ty_slices[signature.args]
                .iter()
                .map(|&ty| &self.interner[self.typec.types.id(ty)])
                .enumerate()
                .map(|(i, str)| format!("var{}: {}", i, str))
                .intersperse(String::from(", "))
                .collect::<String>(),
            &self.interner[self.typec.types.id(signature.ret)],
        )?;

        if let Some(tir) = tir {
            self.display_tir(
                tir,
                buffer,
                0,
                &mut self.typec.ty_slices[signature.args].len(),
            )?;
        } else {
            buffer.push_str("extern");
        }

        Ok(())
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
            TirNode::Call(_) => todo!(),
            TirNode::Access(AccessTir { var, .. }) => {
                write!(buffer, "var{}", var.index())?;
            }
        }

        Ok(())
    }
}
