use mir_t::*;
use storage::{FragRef, Interner, PushMap, ShadowMap, SmallVec, VRef, VRefSlice};
use typec_t::*;

use crate::{Gen, GenLayouts};

pub type IValue = i64;

pub struct Interpreter<'ctx, 'ext> {
    pub ctx: &'ctx mut InterpreterCtx,
    pub typec: &'ctx mut Typec,
    pub interner: &'ctx mut Interner,
    pub mir: &'ext Mir,
    pub layouts: &'ctx mut GenLayouts,
    pub current: &'ctx mut StackFrame,
    pub gen: &'ext Gen,
}

impl<'ctx, 'ext> Interpreter<'ctx, 'ext> {
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
        let (inst, func) = self.load_inst()?;

        let (value, dest) = match inst {
            InstMir::Var(init, dest) => (self.current.values[init], dest),
            InstMir::Int(i, ret) => (Some(i), ret),
            InstMir::Float(f, ret) => (Some(f.to_bits() as i64), ret),
            InstMir::Access(target, dest) => {
                if let Some(dest) = dest {
                    (self.current.values[target], dest)
                } else {
                    self.current.instr += 1;
                    return Ok(());
                }
            }
            InstMir::ConstAccess(c, ret) => (self.gen.get_const(c), ret),
            InstMir::Call(call, ret) => (self.call(call, &func)?, ret),
            InstMir::Ctor(_, _, _) => todo!(),
            InstMir::Deref(_, _) => todo!(),
            InstMir::Ref(_, _) => todo!(),
            InstMir::Field(_, _, _) => todo!(),
            InstMir::Drop(_) => todo!(),
        };

        self.current.values[dest] = value;
        self.current.instr += 1;

        Ok(())
    }

    fn call(
        &mut self,
        call: VRef<CallMir>,
        view: &FuncMirView,
    ) -> Result<Option<IValue>, StepResult> {
        let CallMir {
            callable,
            params,
            args,
        } = view.calls[call];

        let func = match callable {
            CallableMir::Func(func) => func,
            CallableMir::SpecFunc(_) => todo!(),
            CallableMir::Pointer(_) => todo!(),
        };

        if self.typec[func].flags.contains(FuncFlags::BUILTIN) {
            return self.call_builtin(func, params, args, view);
        }

        todo!()
    }

    fn call_builtin(
        &mut self,
        func: FragRef<Func>,
        params: VRefSlice<MirTy>,
        args: VRefSlice<ValueMir>,
        view: &FuncMirView,
    ) -> Result<Option<IValue>, StepResult> {
        let Func {
            name, signature, ..
        } = self.typec[func];

        let op_str = name
            .get(self.interner)
            .split_whitespace()
            .nth(1)
            .unwrap_or(name.get(self.interner));

        let args = view.value_args[args]
            .iter()
            .filter_map(|&arg| self.current.values[arg])
            .collect::<SmallVec<[_; 2]>>();

        let signed = signature.ret.is_signed();
        let float = signature.ret.is_float();

        let value = match op_str {
            "sizeof" => {
                let ty = self.current.types[view.ty_params[params][0]].ty;
                let size =
                    self.layouts
                        .ty_layout(ty, &self.current.params, self.typec, self.interner);
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

            [_a] => todo!(),

            ref other => unimplemented!("{}({:?})", op_str, other),
        }
    }

    fn load_inst(&mut self) -> Result<(InstMir, FuncMirView<'ext>), StepResult> {
        let view = self
            .current
            .func
            .view(&self.mir.modules[self.current.func.module()]);
        self.load_inst_low(&view).map(|inst| (inst, view))
    }

    fn load_inst_low(&mut self, view: &FuncMirView) -> Result<InstMir, StepResult> {
        let block = &view.blocks[self.current.block];

        let Some(&inst) = view.insts[block.insts].get(self.current.instr as usize) else {
            return self.control_flow(view, block);
        };

        Ok(inst)
    }

    fn control_flow(
        &mut self,
        view: &FuncMirView,
        block: &BlockMir,
    ) -> Result<InstMir, StepResult> {
        match block.control_flow {
            ControlFlowMir::Split {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.current.load_register(cond);
                let new_block = [then, otherwise][cond as usize];
                self.current.block = new_block;
                self.current.instr = 0;

                self.load_inst_low(view)
            }
            ControlFlowMir::Goto { dest, ret } => {
                self.current.block = dest;
                self.current.instr = 0;

                if let Some(ret) = ret {
                    let block = &view.blocks[dest];
                    let arg = block
                        .passed
                        .expect("block must have passed value if goto passes a value");
                    self.current.values[arg] = self.current.values[ret];
                }

                self.load_inst_low(view)
            }
            ControlFlowMir::Return(value) => Err(StepResult::Return(self.current.values[value])),
            ControlFlowMir::Terminal => unreachable!(),
        }
    }
}

#[derive(Default)]
pub struct InterpreterCtx {
    _stack: Vec<u8>,
    _frames: Vec<StackFrame>,
    pub fuel: usize,
}

pub struct StackFrame {
    pub params: Vec<Ty>,
    pub block: VRef<BlockMir>,
    pub values: ShadowMap<ValueMir, Option<IValue>>,
    pub types: PushMap<MirTy>,
    pub instr: u32,
    pub frame_base: u32,
    pub func: FuncMir,
}

impl StackFrame {
    fn load_register(&self, reg: VRef<ValueMir>) -> i64 {
        let Some(value) = self.values[reg] else {
            unreachable!()
        };

        value
    }

    fn _value_type(&self, value: VRef<ValueMir>, module: &FuncMirView) -> Ty {
        self.types[module.values[value].ty()].ty
    }
}

pub enum InterpreterError {
    OutOfFuel,
}

enum StepResult {
    Return(Option<IValue>),
}
