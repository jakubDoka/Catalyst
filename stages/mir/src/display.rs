use std::{
    fmt::{self, Write},
    iter,
};

use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl MirChecker<'_> {
    pub fn display_funcs(&self, funcs: &[FragRef<Func>], buffer: &mut String) -> fmt::Result {
        for &func in funcs {
            let mir = self
                .mir
                .bodies
                .get(&func)
                .expect("Expected body to be present");
            self.display_func(func, mir, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    fn display_func(&self, func: FragRef<Func>, mir: &FuncMir, buffer: &mut String) -> fmt::Result {
        self.typec.display_sig(func, self.interner, buffer)?;
        buffer.push_str(" {\n");

        for (i, block) in mir.blocks.values().enumerate() {
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
                    self.typec
                        .display_ty(func.types[func.values[arg].ty].ty, self.interner)
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
                if let Some(ret) = ret {
                    write!(buffer, " var{}", ret.index())?;
                }
            }
            ControlFlowMir::Terminal => {
                buffer.extend(ident.clone());
                buffer.extend(ident.clone());
                write!(buffer, "exit")?;
            }
            ControlFlowMir::Split(cond, then, otherwise) => {
                buffer.extend(ident.clone());
                buffer.extend(ident.clone());
                write!(
                    buffer,
                    "split var{} block{} block{}",
                    cond.index(),
                    then.index(),
                    otherwise.index()
                )?;
            }
            ControlFlowMir::Goto(target, with) => {
                buffer.extend(ident.clone());
                buffer.extend(ident.clone());
                write!(buffer, "goto block{}", target.index())?;
                if let Some(with) = with {
                    write!(buffer, " with var{}", with.index())?;
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
                write!(buffer, "var{} = {}", ret.index(), value)?;
            }
            InstMir::Access(access, ret) => {
                if let Some(ret) = ret {
                    write!(buffer, "var{} = ", ret.index())?;
                }
                write!(buffer, "access var{}", access.index())?;
            }
            InstMir::Call(call, ret) => {
                if let Some(ret) = ret {
                    write!(buffer, "var{} = ", ret.index())?;
                }

                let CallMir {
                    callable,
                    params,
                    args,
                } = func.calls[call];

                match callable {
                    CallableMir::Func(func) => {
                        buffer.push_str(&self.interner[self.typec[func].name])
                    }
                    CallableMir::SpecFunc(bound_func) => {
                        let SpecFunc { name, parent, .. } = self.typec.spec_funcs[bound_func];
                        let bound_id = self.typec.display_spec(Spec::Base(parent), self.interner);
                        write!(buffer, "{}\\{}", bound_id, &self.interner[name])?;
                    }
                    CallableMir::Pointer(ptr) => write!(buffer, "val{}", ptr.index())?,
                }

                if !params.is_empty() {
                    buffer.push('[');

                    let iter = func.ty_params[params]
                        .iter()
                        .map(|&ty| func.types[ty].ty)
                        .map(|ty| self.typec.display_ty(ty, self.interner))
                        .intersperse(", ".into())
                        .collect::<String>();
                    buffer.push_str(&iter);

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
            InstMir::Const(r#const, value) => {
                write!(buffer, "var{} = const {}", value.index(), r#const.index())?;
            }
            InstMir::Ctor(fields, value, ..) => {
                write!(buffer, "var{} =", value.index())?;

                buffer.push('{');
                if let Some((first, others)) = func.value_args[fields].split_first() {
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
            InstMir::Bool(value, ret) => {
                write!(buffer, "var{} = {}", ret.index(), value)?;
            }
            InstMir::Var(value, ret) => {
                write!(buffer, "var{} = var{}", ret.index(), value.index())?;
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
        match ty {
            Ty::Struct(s) => {
                let Struct { fields, .. } = self.typec[s];
                res.push_str("\\{ ");
                if let Some((&first, rest)) = self.typec[fields].split_first() {
                    write!(res, "{}: ", &self.interner[first.name]).unwrap();
                    self.display_pat_low(first.ty, params, res, frontier);
                    for &field in rest {
                        res.push_str(", ");
                        write!(res, "{}: ", &self.interner[field.name]).unwrap();
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
                let Pointer { base, .. } = self.typec[ptr];
                res.push('^');
                self.display_pat_low(base, params, res, frontier)
            }
            Ty::Param(index) => {
                let ty = params[index as usize];
                self.display_pat_low(ty, params, res, frontier)
            }
            Ty::Builtin(b) => match b {
                Builtin::Unit
                | Builtin::Terminal
                | Builtin::Uint
                | Builtin::U32
                | Builtin::U16
                | Builtin::U8 => {
                    let first = advance();
                    write!(res, "{}", first).unwrap();
                }
                Builtin::Char => todo!(),
                Builtin::Bool => {
                    let first = advance();
                    if first == Range::full() {
                        res.push('_');
                    } else {
                        write!(res, "{}", first.start == 1).unwrap();
                    }
                }
            },
            Ty::Enum(r#enum) => {
                let Enum { variants, .. } = self.typec[r#enum];
                let variant = if self.typec.get_enum_flag_ty(r#enum).is_some() {
                    let flag = advance();
                    let index = flag.start as usize;
                    self.typec[variants][index]
                } else {
                    self.typec[variants][0]
                };
                write!(res, "\\{}", &self.interner[variant.name]).unwrap();
                if variant.ty != Ty::UNIT {
                    res.push('~');
                    self.display_pat_low(variant.ty, params, res, frontier);
                }
            }
        }
    }
}
