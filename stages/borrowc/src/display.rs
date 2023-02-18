use std::{
    fmt::{self, Write},
    iter,
};

use mir::*;
use resources::Resources;
use storage::*;
use types::*;
use type_creator::TypeDisplay;

pub fn display_function(
    funcs: impl IntoIterator<Item = FragRef<Func>>,
    ctx: MirDisplayCtx,
) -> String {
    let mut buffer = String::new();
    ctx.display_funcs(funcs, &mut buffer).unwrap();
    buffer
}

pub struct MirDisplayCtx<'i> {
    pub types: &'i Types,
    pub module: &'i ModuleMir,
    pub interner: &'i Interner,
    pub resources: &'i Resources,
    pub mir: &'i Mir,
}

impl MirDisplayCtx<'_> {
    pub fn display_funcs(
        &self,
        funcs: impl IntoIterator<Item = FragRef<Func>>,
        buffer: &mut String,
    ) -> fmt::Result {
        for func in funcs {
            let mir = self
                .mir
                .bodies
                .get(&BodyOwner::Func(func))
                .expect("Expected body to be present");
            self.display_func(mir.view(self.module), func, buffer)?;
            buffer.push_str("\n\n");
        }

        Ok(())
    }

    fn display_func(
        &self,
        mir: FuncMirView,
        func: FragRef<Func>,
        buffer: &mut String,
    ) -> fmt::Result {
        self.types[func].display(self.types, self.interner, buffer)?;
        writeln!(
            buffer,
            " {{ ({}) ret var{}",
            mir.args
                .iter()
                .map(|&arg| format!(
                    "var{}: {}",
                    arg.index(),
                    type_creator::display(self.types, self.interner, mir.value_ty(arg))
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
            match mir.blocks[block].control_flow {
                ControlFlowMir::Split {
                    then, otherwise, ..
                } => blocks.extend([then, otherwise]),
                ControlFlowMir::Goto { dest, .. } => blocks.push(dest),
                ControlFlowMir::Return(..) | ControlFlowMir::Terminal => (),
            }
        }

        for (i, block) in blocks.into_iter().map(|b| (b, &mir.blocks[b])) {
            self.display_block(i.index(), &mir, block, buffer)?;
            write!(buffer, "\n\n")?;
        }

        buffer.push('}');

        Ok(())
    }

    fn display_block(
        &self,
        i: usize,
        mir: &FuncMirView,
        block: &BlockMir,
        buffer: &mut String,
    ) -> fmt::Result {
        let ident = iter::repeat(' ').take(4);

        buffer.extend(ident.clone());
        writeln!(buffer, "block{i} {{")?;

        for &inst in &mir.insts[block.insts] {
            buffer.extend(ident.clone());
            buffer.extend(ident.clone());
            self.display_inst(mir, inst, buffer)?;
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

    fn display_inst(&self, mir: &FuncMirView, inst: InstMir, buffer: &mut String) -> fmt::Result {
        match inst {
            InstMir::Int(value, ret) => {
                write!(buffer, "var{} = {}", ret.index(), value)?;
            }
            InstMir::Float(value, ret) => {
                write!(buffer, "var{} = {}", ret.index(), value)?;
            }
            InstMir::Assign(access, ret) => {
                write!(buffer, "var{} = var{}", ret.index(), access.index())?;
            }
            InstMir::Call(call, ret) => {
                write!(buffer, "var{} = ", ret.index())?;

                let CallMir {
                    callable,
                    params,
                    args,
                } = mir.calls[call];

                match callable {
                    CallableMir::Func(func) => {
                        buffer.push_str(self.types[func].name.get(self.interner))
                    }
                    CallableMir::SpecFunc(bound_func) => {
                        let SpecFunc { name, parent, .. } = self.types[bound_func];
                        let bound_id =
                            type_creator::display(self.types, self.interner, Spec::Base(parent));
                        write!(buffer, "{}\\{}", bound_id, name.get(self.interner))?;
                    }
                    CallableMir::Pointer(ptr) => write!(buffer, "val{}", ptr.index())?,
                }

                if !params.is_empty() {
                    buffer.push('[');
                    let iter = mir.ty_params[params]
                        .iter()
                        .map(|&ty| mir.types[ty].ty)
                        .map(|ty| type_creator::display(self.types, self.interner, ty))
                        .intersperse(", ".into())
                        .collect::<String>();
                    buffer.push_str(&iter);

                    buffer.push(']');
                }

                buffer.push('(');

                if let Some((first, others)) = mir.value_args[args].split_first() {
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
                if let Some((first, others)) = mir.value_args[fields].split_first() {
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
                    self.types[r#const].name.get(self.interner)
                )?;
            }
            InstMir::Drop(drop) => {
                let DropMir { value } = mir.drops[drop];
                write!(buffer, "drop var{}", value.index())?;
            }
        }

        Ok(())
    }
}
