use std::{
    fmt::{self, Write},
    iter,
};

use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl MirChecker<'_, '_> {
    pub fn display_funcs(&self, buffer: &mut String) -> fmt::Result {
        for &func in &self.mir_ctx.just_compiled_funcs {
            let mir = self
                .mir
                .bodies
                .get(&BodyOwner::Func(func))
                .expect("Expected body to be present");
            self.display_func(&mir, func, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    pub fn dbg_funcs(&self) {
        let mut buffer = String::new();
        self.display_funcs(&mut buffer).unwrap();
        println!("{buffer}");
    }

    fn display_func(&self, mir: &FuncMir, func: FragRef<Func>, buffer: &mut String) -> fmt::Result {
        self.typec.display_sig(func, self.interner, buffer)?;
        writeln!(
            buffer,
            " {{ ({}) ret var{}",
            self.mir_ctx.module.value_args[mir.args]
                .iter()
                .map(|&arg| format!(
                    "var{}: {}",
                    arg.index(),
                    self.typec
                        .display_ty(mir.value_ty(arg, &self.mir_ctx.module), self.interner)
                ))
                .collect::<Vec<_>>()
                .join(", "),
            mir.ret.index()
        )?;

        let mut seen = BitSet::new();
        let mut blocks = vec![mir.entry];
        let mut cursor = 0;
        while let Some(&block) = blocks.get(cursor) {
            if !seen.insert(block.index()) {
                blocks.swap_remove(cursor);
                continue;
            }
            cursor += 1;
            match self.mir_ctx.module.blocks[block].control_flow {
                ControlFlowMir::Split {
                    then, otherwise, ..
                } => blocks.extend([then, otherwise]),
                ControlFlowMir::Goto { dest, .. } => blocks.push(dest),
                ControlFlowMir::Return(..) | ControlFlowMir::Terminal => (),
            }
        }

        for (i, block) in blocks
            .into_iter()
            .map(|b| (b, &self.mir_ctx.module.blocks[b]))
        {
            self.display_block(i.index(), mir, block, buffer)?;
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
        writeln!(buffer, "block{i} {{")?;

        for &inst in &self.mir_ctx.module.insts[block.insts] {
            buffer.extend(ident.clone());
            buffer.extend(ident.clone());
            self.display_inst(func, inst, buffer)?;
            buffer.push('\n');
        }

        buffer.extend(ident.clone());
        buffer.extend(ident.clone());
        match block.control_flow {
            ControlFlowMir::Return(ret) => {
                write!(buffer, "return var{}", ret.index())?;
            }
            ControlFlowMir::Terminal => {
                write!(buffer, "exit")?;
            }
            ControlFlowMir::Split {
                cond,
                then,
                otherwise,
            } => {
                write!(
                    buffer,
                    "split var{} block{} block{}",
                    cond.index(),
                    then.index(),
                    otherwise.index()
                )?;
            }
            ControlFlowMir::Goto { dest, ret } => {
                write!(buffer, "goto block{}", dest.index())?;
                if let Some(ret) = ret {
                    write!(buffer, " with var{}", ret.index())?;
                }
            }
        }

        buffer.push('\n');
        buffer.extend(ident);
        buffer.push('}');

        Ok(())
    }

    fn display_inst(&self, func: &FuncMir, inst: InstMir, buffer: &mut String) -> fmt::Result {
        match inst {
            InstMir::Int(value, ret) => {
                write!(buffer, "var{} = {}", ret.index(), value)?;
            }
            InstMir::Float(value, ret) => {
                write!(buffer, "var{} = {}", ret.index(), value)?;
            }
            InstMir::Access(access, ret) => {
                if let Some(ret) = ret {
                    write!(buffer, "var{} = ", ret.index())?;
                }
                write!(buffer, "access var{}", access.index())?;
            }
            InstMir::Call(call, ret) => {
                write!(buffer, "var{} = ", ret.index())?;

                let CallMir {
                    callable,
                    params,
                    args,
                } = self.mir_ctx.module.calls[call];

                match callable {
                    CallableMir::Func(func) => {
                        buffer.push_str(self.typec[func].name.get(self.interner))
                    }
                    CallableMir::SpecFunc(bound_func) => {
                        let SpecFunc { name, parent, .. } = self.typec[bound_func];
                        let bound_id = self.typec.display_spec(Spec::Base(parent), self.interner);
                        write!(buffer, "{}\\{}", bound_id, name.get(self.interner))?;
                    }
                    CallableMir::Pointer(ptr) => write!(buffer, "val{}", ptr.index())?,
                }

                if !params.is_empty() {
                    buffer.push('[');
                    let iter = self.mir_ctx.module.ty_params[params]
                        .iter()
                        .map(|&ty| func.ty(ty, &self.mir_ctx.module))
                        .map(|ty| self.typec.display_ty(ty, self.interner))
                        .intersperse(", ".into())
                        .collect::<String>();
                    buffer.push_str(&iter);

                    buffer.push(']');
                }

                buffer.push('(');

                if let Some((first, others)) = self.mir_ctx.module.value_args[args].split_first() {
                    write!(buffer, "val{}", first.index())?;

                    for &other in others {
                        write!(buffer, ", val{}", other.index())?;
                    }
                }

                buffer.push(')');
            }
            InstMir::Ctor(fields, value, ..) => {
                write!(buffer, "var{} =", value.index())?;

                buffer.push('{');
                if let Some((first, others)) = self.mir_ctx.module.value_args[fields].split_first()
                {
                    write!(buffer, "var{}", first.index())?;

                    for &other in others {
                        write!(buffer, ", var{}", other.index())?;
                    }
                }
                buffer.push('}');
            }
            InstMir::Deref(target, value) => {
                write!(buffer, "var{} = *var{}", value.index(), target.index())?;
            }
            InstMir::Ref(target, value) => {
                write!(buffer, "var{} = &var{}", value.index(), target.index())?;
            }
            InstMir::Field(header, field, value) => {
                write!(
                    buffer,
                    "var{} = var{}.{}",
                    value.index(),
                    header.index(),
                    field,
                )?;
            }
            InstMir::Var(value, ret) => {
                write!(buffer, "var{} = var{}", ret.index(), value.index())?;
            }
            InstMir::ConstAccess(r#const, ret) => {
                write!(
                    buffer,
                    "var{} = const_access {}",
                    ret.index(),
                    self.typec[r#const].name.get(self.interner)
                )?;
            }
            InstMir::Drop(drop) => {
                let DropMir { value } = self.mir_ctx.module.drops[drop];
                write!(buffer, "drop var{}", value.index())?;
            }
        }

        Ok(())
    }

    pub fn display_pat(&self, pat: &[Range], ty: Ty) -> String {
        let mut res = String::new();
        let mut frontier = pat;
        self.display_pat_low(ty, &[], &mut res, &mut frontier);
        res
    }

    fn display_pat_low(&self, ty: Ty, params: &[Ty], res: &mut String, frontier: &mut &[Range]) {
        let mut advance = || {
            let (&first, others) = frontier.split_first().unwrap_or((&Range::full(), &[]));
            *frontier = others;
            first
        };
        use Builtin::*;
        match ty {
            Ty::Struct(s) => {
                let Struct { fields, .. } = self.typec[s];
                res.push_str("\\{ ");
                if let Some((&first, rest)) = self.typec[fields].split_first() {
                    write!(res, "{}: ", first.name.get(self.interner)).unwrap();
                    self.display_pat_low(first.ty, params, res, frontier);
                    for &field in rest {
                        res.push_str(", ");
                        write!(res, "{}: ", field.name.get(self.interner)).unwrap();
                        self.display_pat_low(field.ty, params, res, frontier);
                    }
                }
                res.push_str(" }");
            }
            Ty::Instance(inst) => {
                let Instance { args, base } = self.typec[inst];
                let params = &self.typec[args];
                self.display_pat_low(base.as_ty(), params, res, frontier)
            }
            Ty::Pointer(ptr) => {
                let base = self.typec[ptr.ty()];
                res.push('^');
                write!(res, "{}", ptr.mutability.to_mutability()).unwrap();
                self.display_pat_low(base, params, res, frontier)
            }
            Ty::Param(index) => {
                let ty = params[index as usize];
                self.display_pat_low(ty, params, res, frontier)
            }
            Ty::Builtin(b) => match b {
                Unit | Terminal | Uint | U32 | Mutable | Immutable | U16 | U8 | Short | Cint
                | Long | LongLong => {
                    let first = advance();
                    write!(res, "{first}").unwrap();
                }
                Char => todo!(),
                Bool => {
                    let first = advance();
                    if first == Range::full() {
                        res.push('_');
                    } else {
                        write!(res, "{}", first.start == 1).unwrap();
                    }
                }
                F32 | F64 => unreachable!(),
            },
            Ty::Enum(r#enum) => {
                let Enum { variants, .. } = self.typec[r#enum];
                let variant = if self.typec.enum_flag_ty(r#enum) != Uint {
                    let flag = advance();
                    let index = flag.start as usize;
                    self.typec[variants][index]
                } else {
                    self.typec[variants][0]
                };
                write!(res, "\\{}", variant.name.get(self.interner)).unwrap();
                if variant.ty != Ty::UNIT {
                    res.push('~');
                    self.display_pat_low(variant.ty, params, res, frontier);
                }
            }
        }
    }
}
