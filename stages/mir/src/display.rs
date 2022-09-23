use std::{
    fmt::{self, Write},
    iter,
};

use mir_t::*;
use packaging_t::span_str;
use storage::*;
use typec_t::*;

use crate::{builder::MirFuncs, *};

impl MirChecker<'_> {
    pub fn display_funcs(&self, funcs: &MirFuncs, buffer: &mut String) -> fmt::Result {
        for &(func, ref mir) in funcs.iter() {
            self.display_func(func, mir, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    fn display_func(&self, func: VRef<Func>, mir: &FuncMir, buffer: &mut String) -> fmt::Result {
        self.typec.display_sig(func, self.interner, buffer)?;
        buffer.push_str(" {\n");

        for (i, block) in mir.blocks.iter().enumerate() {
            self.display_block(i, mir, block, buffer)?;
            write!(buffer, "\n\n")?;
        }

        buffer.push('}');

        Ok(())
    }

    fn display_block(
        &self,
        i: usize,
        func: &FuncMir,
        block: &BlockMir,
        buffer: &mut String,
    ) -> fmt::Result {
        let ident = iter::repeat(' ').take(4);

        buffer.extend(ident.clone());
        writeln!(
            buffer,
            "block{}({}) {{",
            i,
            func.value_args[block.args]
                .iter()
                .map(|&arg| format!(
                    "var{}: {}",
                    arg.index(),
                    &self.interner[self.typec.types.id(func.value_ty(arg))]
                ))
                .collect::<Vec<_>>()
                .join(", "),
        )?;

        for &inst in &func.insts[block.insts] {
            buffer.extend(ident.clone());
            buffer.extend(ident.clone());
            self.display_inst(inst, func, buffer)?;
            buffer.push('\n');
        }

        match block.control_flow {
            ControlFlowMir::Return(ret) => {
                buffer.extend(ident.clone());
                buffer.extend(ident.clone());
                write!(buffer, "return")?;
                if let Some(ret) = ret.expand() {
                    write!(buffer, " var{}", ret.index())?;
                }
            }
        }

        buffer.push('\n');
        buffer.extend(ident);
        buffer.push('}');

        Ok(())
    }

    fn display_inst(&self, inst: InstMir, _func: &FuncMir, buffer: &mut String) -> fmt::Result {
        if let Some(value) = inst.value.expand() {
            write!(buffer, "var{} = ", value.index())?;
        }

        match inst.kind {
            InstKind::Int(span) => {
                write!(buffer, "int {}", span_str!(self, span))?;
            }
            InstKind::Access(access) => {
                write!(buffer, "access var{}", access.index())?;
            }
        }

        Ok(())
    }
}
