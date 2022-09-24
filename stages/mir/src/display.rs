use std::{
    fmt::{self, Write},
    iter,
};

use mir_t::*;
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

    fn display_inst(&self, inst: InstMir, func: &FuncMir, buffer: &mut String) -> fmt::Result {
        match inst {
            InstMir::Int(value, ret) => {
                write!(buffer, "var{} = int {}", ret.index(), value)?;
            }
            InstMir::Access(access) => {
                write!(buffer, "access var{}", access.index())?;
            }
            InstMir::Call(
                CallMir {
                    callable,
                    params,
                    args,
                },
                ret,
            ) => {
                if ret != ValueMir::UNIT {
                    write!(buffer, "var{} = ", ret.index())?;
                }

                match callable {
                    CallableMir::Func(func) => {
                        buffer.push_str(&self.interner[self.typec.funcs.id(func)])
                    }
                    CallableMir::BoundFunc(bound_func) => {
                        let BoundFunc { loc, parent, .. } = self.typec.bound_funcs[bound_func];
                        let bound_id = self.typec.bounds.id(parent);
                        write!(
                            buffer,
                            "{}\\{}",
                            &self.interner[bound_id], &self.interner[loc.name]
                        )?;
                    }
                    CallableMir::Pointer(ptr) => write!(buffer, "val{}", ptr.index())?,
                }

                if !params.is_empty() {
                    buffer.push('[');

                    let iter = func.ty_params[params]
                        .iter()
                        .map(|&ty| func.dependant_types[ty].ty)
                        .map(|ty| self.typec.types.id(ty))
                        .map(|ident| &self.interner[ident])
                        .intersperse(", ")
                        .flat_map(str::chars);
                    buffer.extend(iter);

                    buffer.push(']');
                }

                buffer.push('(');

                if let Some((first, others)) = func.value_args[args].split_first() {
                    write!(buffer, "val{}", first.index())?;

                    for &other in others {
                        write!(buffer, ", val{}", other.index())?;
                    }
                }

                buffer.push(')');
            }
        }

        Ok(())
    }
}
