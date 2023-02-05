use mir_t::*;
use storage::{FragRef, Interner, ShadowMap, SmallVec, VRef, VRefSlice};
use typec_t::*;

use crate::GenLayouts;

type IValue = i64;

pub struct Interpreter<'ctx> {
    pub ctx: &'ctx mut InterpreterCtx,
    pub typec: &'ctx mut Typec,
    pub interner: &'ctx mut Interner,
    pub mir: &'ctx Mir,
    pub layouts: &'ctx mut GenLayouts,
}

impl<'ctx> Interpreter<'ctx> {
    pub fn interpret(&mut self) -> Result<Option<IValue>, InterpreterError> {
        while let Some(next_fuel) = self.ctx.fuel.checked_sub(1) {
            self.ctx.fuel = next_fuel;

            let Err(err) = self.step() else {continue};

            match err {
                StepResult::Return(value) => return Ok(value),
            }
        }

        Err(InterpreterError::OutOfFuel)
    }

    fn step(&mut self) -> Result<(), StepResult> {
        let inst = self.load_inst()?;

        let (value, dest) = match inst {
            InstMir::Var(init, dest) => (self.ctx.current.values[init], dest),
            InstMir::Int(i, ret) => (Some(i), ret),
            InstMir::Float(f, ret) => (Some(f.to_bits() as i64), ret),
            InstMir::Access(target, dest) => {
                if let Some(dest) = dest {
                    (self.ctx.current.values[target], dest)
                } else {
                    self.ctx.current.instr += 1;
                    return Ok(());
                }
            }
            InstMir::ConstAccess(_, _) => todo!(),
            InstMir::Call(call, ret) => (self.call(call)?, ret),
            InstMir::Ctor(_, _, _) => todo!(),
            InstMir::Deref(_, _) => todo!(),
            InstMir::Ref(_, _) => todo!(),
            InstMir::Field(_, _, _) => todo!(),
            InstMir::Drop(_) => todo!(),
        };

        self.ctx.current.values[dest] = value;
        self.ctx.current.instr += 1;

        Ok(())
    }

    fn call(&mut self, call: VRef<CallMir>) -> Result<Option<IValue>, StepResult> {
        let module = &self.mir.modules[self.ctx.current.func.module];
        let CallMir {
            callable,
            params,
            args,
        } = module.calls[self.ctx.current.func.calls][call.index()];

        let func = match callable {
            CallableMir::Func(func) => func,
            CallableMir::SpecFunc(_) => todo!(),
            CallableMir::Pointer(_) => todo!(),
        };

        if self.typec[func].flags.contains(FuncFlags::BUILTIN) {
            return self.call_builtin(func, params, args, module);
        }

        todo!()
    }

    fn call_builtin(
        &mut self,
        func: FragRef<Func>,
        params: VRefSlice<MirTy>,
        args: VRefSlice<ValueMir>,
        module: &ModuleMir,
    ) -> Result<Option<IValue>, StepResult> {
        let Func {
            name, signature, ..
        } = self.typec[func];

        let op_str = name
            .get(self.interner)
            .split_whitespace()
            .nth(1)
            .unwrap_or(name.get(self.interner));

        let args = module.value_args[args]
            .iter()
            .filter_map(|&arg| self.ctx.current.values[arg])
            .collect::<SmallVec<[_; 2]>>();

        let signed = signature.ret.is_signed();
        let float = signature.ret.is_float();

        let value = match op_str {
            "sizeof" => {
                let ty = self.ctx.current.types[module.ty_params[params][0]].ty;
                let size =
                    self.layouts
                        .ty_layout(ty, &self.ctx.current.params, self.typec, self.interner);
                size.size as i64
            }
            "cast" => args[0],
            _ => Self::dispatch_op(op_str, args.as_slice(), float, signed),
        };

        Ok(Some(value))
    }

    fn dispatch_op(op_str: &str, args: &[i64], float: bool, signed: bool) -> i64 {
        match *args {
            [a, b] if float => {
                let a = f64::from_bits(a as u64);
                let b = f64::from_bits(b as u64);
                let r = match op_str {
                    "+" => a + b,
                    "-" => a - b,
                    "*" => a * b,
                    "/" => a / b,
                    "==" => return (a == b) as i64,
                    "!=" => return (a != b) as i64,
                    "<" => return (a < b) as i64,
                    "<=" => return (a <= b) as i64,
                    ">" => return (a > b) as i64,
                    ">=" => return (a >= b) as i64,
                    other => unimplemented!("{}({:?}, {:?})", other, a, b),
                };
                r.to_bits() as i64
            }
            [a, b] => match op_str {
                "+" => a.wrapping_add(b),
                "-" => a.wrapping_sub(b),
                "*" => a.wrapping_mul(b),
                "/" if signed => a.wrapping_div(b),
                "/" => (a as u64).wrapping_div(b as u64) as i64,
                "%" if signed => a.wrapping_rem(b),
                "%" => (a as u64).wrapping_rem(b as u64) as i64,
                "==" => (a == b) as i64,
                "!=" => (a != b) as i64,
                "<" if signed => (a < b) as i64,
                "<" => ((a as u64) < (b as u64)) as i64,
                "<=" if signed => (a <= b) as i64,
                "<=" => ((a as u64) <= (b as u64)) as i64,
                ">" if signed => (a > b) as i64,
                ">" => ((a as u64) > (b as u64)) as i64,
                ">=" if signed => (a >= b) as i64,
                ">=" => ((a as u64) >= (b as u64)) as i64,
                "<<" => a.wrapping_shl(b as u32),
                ">>" if signed => a.wrapping_shr(b as u32),
                ">>" => (a as u64).wrapping_shr(b as u32) as i64,
                "&" => a & b,
                "|" => a | b,
                "^" => a ^ b,
                other => unimplemented!("{}({:?}, {:?})", other, a, b),
            },

            [a] => todo!(),

            ref other => unimplemented!("{}({:?})", op_str, other),
        }
    }

    fn load_inst(&mut self) -> Result<InstMir, StepResult> {
        let module = &self.mir.modules[self.ctx.current.func.module];
        let block = &module.blocks[self.ctx.current.block];

        let Some(&inst) = module.insts[block.insts].get(self.ctx.current.instr as usize) else {
            return self.control_flow(module, block);
        };

        Ok(inst)
    }

    fn control_flow(
        &mut self,
        module: &ModuleMir,
        block: &BlockMir,
    ) -> Result<InstMir, StepResult> {
        match block.control_flow {
            ControlFlowMir::Split {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.ctx.load_register(cond);
                let new_block = [then, otherwise][cond as usize];
                self.ctx.current.block = new_block;
                self.ctx.current.instr = 0;

                self.load_inst()
            }
            ControlFlowMir::Goto { dest, ret } => {
                self.ctx.current.block = dest;
                self.ctx.current.instr = 0;

                if let Some(ret) = ret {
                    let block = &module.blocks[dest];
                    let arg = block
                        .passed
                        .expect("block must have passed value if goto passes a value");
                    self.ctx.current.values[arg] = self.ctx.current.values[ret];
                }

                self.load_inst()
            }
            ControlFlowMir::Return(value) => {
                Err(StepResult::Return(self.ctx.current.values[value]))
            }
            ControlFlowMir::Terminal => unreachable!(),
        }
    }
}

pub struct InterpreterCtx {
    stack: Vec<u8>,
    frames: Vec<StackFrame>,
    fuel: usize,
    current: StackFrame,
}

impl InterpreterCtx {
    fn load_register(&mut self, reg: VRef<ValueMir>) -> i64 {
        let Some(value) = self.current.values[reg] else {
            unreachable!()
        };

        value
    }

    fn value_type(&mut self, value: VRef<ValueMir>, module: &ModuleMir) -> Ty {
        let ValueMir { ty, .. } = module.values[value];
        self.current.types[ty].ty
    }
}

struct StackFrame {
    params: Vec<Ty>,
    block: VRef<BlockMir>,
    values: ShadowMap<ValueMir, Option<IValue>>,
    types: FuncTypes,
    instr: u32,
    frame_base: u32,
    func: FuncMir,
}

pub enum InterpreterError {
    OutOfFuel,
}

enum StepResult {
    Return(Option<IValue>),
}
