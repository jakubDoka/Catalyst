use std::{
    default::default,
    mem,
    ops::{Deref, DerefMut},
    ptr::copy_nonoverlapping,
};

use mir_t::*;
use storage::{FragRef, Interner, PushMap, ShadowMap, SmallVec, VRef, VRefSlice};
use typec_t::*;

use crate::{context::ComputedConst, Gen, GenLayouts, Layout};

pub type IRegister = i64;

pub struct Interpreter<'ctx, 'ext> {
    pub ctx: PreparedInterpreterCtx<'ctx>,
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
                StepError::Return(data) => {
                    if let Some(pushed) = self.ctx.frames.pop() {
                        self.pop_call(pushed, data);
                    } else {
                        return Ok(data.and_then(|data| self.current.unpack_slot_low(data)));
                    }
                }
                StepError::Call => (),
                StepError::Inerpreter(err) => return Err(err),
            }
        }

        Err(InterpreterError::OutOfFuel)
    }

    fn pop_call(&mut self, mut pushed: PushedFrame, value: Option<ISlot>) {
        self.ctx.stack.truncate(self.current.frame_base as usize);
        pushed.frame.values[pushed.return_value] = value;
        *self.current = pushed.frame;
        // when pus_call happens the instr of caller is
        // not advanced
        self.current.instr += 1;
    }

    fn push_call(
        &mut self,
        view: &FuncMirView,
        func: FragRef<Func>,
        params: VRefSlice<MirTy>,
        args: VRefSlice<ValueMir>,
        return_value: VRef<ValueMir>,
    ) -> Result<(), StepError> {
        let body = self
            .mir
            .bodies
            .get(&BodyOwner::Func(func))
            .ok_or(StepError::Inerpreter(InterpreterError::MissingBody))?
            .to_owned();
        let module = &self.mir.modules[body.module()];
        let next_view = body.view(module);

        let mut frame = StackFrame {
            params: view.ty_params[params]
                .iter()
                .map(|&ty| self.current.types[ty].ty)
                .collect(),
            block: body.entry(),
            values: default(),
            offsets: default(),
            types: default(),
            instr: 0,
            frame_base: self.ctx.stack.len() as u32,
            func: body,
        };

        mir_t::swap_mir_types(
            &next_view,
            &mut frame.types,
            &frame.params,
            self.typec,
            self.interner,
        );

        let frame = mem::replace(self.current, frame);
        for (&source, &targer) in view.value_args[args].iter().zip(next_view.args) {
            let Some(source_value) = frame.unpack_slot(source) else {continue};
            self.assign(source_value, frame.offsets[source], targer, &next_view)?;
        }

        let ty = self.current.value_ty(return_value, &next_view);
        let layout = self.layout(ty);
        self.ensure_dest(return_value, view, layout)?;
        self.current.values[next_view.ret] = frame.values[return_value];

        self.ctx.frames.push(PushedFrame {
            return_value,
            frame,
        });

        Ok(())
    }

    fn step(&mut self) -> Result<(), StepError> {
        let (inst, func) = self.load_inst()?;

        let ((value, offset), dest) = match inst {
            InstMir::Var(init, dest) => ((self.current.values[init], 0), dest),
            InstMir::Int(i, ret) => ((Some(ISlot::Value(IValue::Register(i))), 0), ret),
            InstMir::Float(f, ret) => (
                (
                    Some(ISlot::Value(IValue::Register(f.to_bits() as IRegister))),
                    0,
                ),
                ret,
            ),
            InstMir::Assign(target, dest) => ((self.current.values[target], 0), dest),
            InstMir::ConstAccess(c, ret) => ((self.constant(c), 0), ret),
            InstMir::Call(call, ret) => ((self.call(call, ret, &func)?, 0), ret),
            InstMir::Ctor(fields, ret) => ((self.ctor(fields, ret, &func)?, 0), ret),
            InstMir::Deref(target, ret) => ((self.dereference(target, ret, &func), 0), ret),
            InstMir::Ref(target, ret) => ((self.reference(target, ret, &func), 0), ret),
            InstMir::Field(target, field, ret) => (self.field(target, field, &func), ret),
            InstMir::Drop(_) => todo!(),
        };

        if let Some(value) = value && let Some(value) = self.current.unpack_slot_low(value) {
            self.assign(value, offset, dest, &func)?;
        }
        self.current.instr += 1;

        Ok(())
    }

    fn constant(&mut self, c: FragRef<Const>) -> Option<ISlot> {
        let slot = self.gen.get_const(c);
        match slot.as_deref()? {
            &ComputedConst::Scalar(s) => Some(ISlot::Value(IValue::Register(s))),
            //ComputedConst::Memory(m) => Some(ISlot::Value(IValue::Memory(m.as_ptr()))),
            ComputedConst::Unit => None,
        }
    }

    fn ctor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        dest: VRef<ValueMir>,
        view: &FuncMirView,
    ) -> Result<Option<ISlot>, StepError> {
        let ty = self.current.value_ty(dest, view);
        let layout = self.layout(ty);
        self.ensure_dest(dest, view, layout)?;

        let base_offset = self.current.offsets[dest];
        for (&value, &offset) in view.value_args[fields]
            .iter()
            .zip(&self.layouts.offsets[layout.offsets])
        {
            self.current.values[value] = Some(ISlot::Variable(dest));
            self.current.offsets[value] = base_offset + offset;
        }

        Ok(None)
    }

    fn call(
        &mut self,
        call: VRef<CallMir>,
        return_value: VRef<ValueMir>,
        view: &FuncMirView,
    ) -> Result<Option<ISlot>, StepError> {
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

        self.push_call(view, func, params, args, return_value)?;
        Err(StepError::Call)
    }

    fn call_builtin(
        &mut self,
        func: FragRef<Func>,
        params: VRefSlice<MirTy>,
        args: VRefSlice<ValueMir>,
        view: &FuncMirView,
    ) -> Result<Option<ISlot>, StepError> {
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
            .map(|&arg| match self.current.unpack_slot(arg) {
                Some(IValue::Register(value)) => value,
                _ => unreachable!(),
            })
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

        Ok(Some(ISlot::Value(IValue::Register(value))))
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

    fn load_inst(&mut self) -> Result<(InstMir, FuncMirView<'ext>), StepError> {
        let view = self
            .current
            .func
            .view(&self.mir.modules[self.current.func.module()]);
        self.load_inst_low(&view).map(|inst| (inst, view))
    }

    fn load_inst_low(&mut self, view: &FuncMirView) -> Result<InstMir, StepError> {
        let block = &view.blocks[self.current.block];

        let Some(&inst) = view.insts[block.insts].get(self.current.instr as usize) else {
            return self.control_flow(view, block);
        };

        Ok(inst)
    }

    fn control_flow(&mut self, view: &FuncMirView, block: &BlockMir) -> Result<InstMir, StepError> {
        match block.control_flow {
            ControlFlowMir::Split {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.current.load_register(cond);
                let new_block = [otherwise, then][cond as usize];
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
            ControlFlowMir::Return(value) => Err(StepError::Return(self.current.values[value])),
            ControlFlowMir::Terminal => unreachable!(),
        }
    }

    fn assign(
        &mut self,
        source: IValue,
        source_offset: u32,
        dest: VRef<ValueMir>,
        view: &FuncMirView,
    ) -> Result<(), StepError> {
        let ty = self.current.value_ty(dest, view);
        let layout = self.layout(ty);

        let dest_value = self.ensure_dest(dest, view, layout)?;

        let mask = || {
            if layout.size == 8 {
                -1
            } else {
                (1 << (layout.size * 8)) - 1
            }
        };

        let dest_offset = self.current.offsets[dest];
        let dest = self.current.root_value(dest);

        let get_bit_field = |value: i64| (value >> (source_offset * 8)) & mask();

        let set_bit_field = |value: i64, dest: i64| {
            let value = value << (dest_offset * 8);
            let mask = mask() << (dest_offset * 8);
            (dest & !mask) | value
        };

        match (source, dest_value) {
            (IValue::Register(val), IValue::Register(dest_value)) => {
                let val = get_bit_field(val);
                let dest_value = dbg!(set_bit_field(dbg!(val), dbg!(dest_value)));
                self.current.values[dest] = Some(ISlot::Value(IValue::Register(dest_value)));
            }
            (IValue::Register(val), IValue::Memory(mem)) => {
                let val = get_bit_field(val);
                unsafe {
                    copy_nonoverlapping(
                        &val as *const _ as *const u8,
                        mem.add(dest_offset as usize),
                        layout.size as usize,
                    );
                }
            }
            (IValue::Memory(mem), IValue::Register(value)) => {
                let mut val = 0;
                unsafe {
                    copy_nonoverlapping(
                        mem.add(source_offset as usize),
                        &mut val as *mut _ as *mut u8,
                        layout.size as usize,
                    );
                }
                let value = set_bit_field(val, value);
                self.current.values[dest] = Some(ISlot::Value(IValue::Register(value)));
            }
            (IValue::Memory(mem_from), IValue::Memory(mem_to)) => unsafe {
                copy_nonoverlapping(
                    mem_from.add(source_offset as usize),
                    mem_to.add(dest_offset as usize),
                    layout.size as usize,
                );
            },
        }

        Ok(())
    }

    fn layout(&mut self, ty: Ty) -> Layout {
        self.layouts.ty_layout(ty, &[], self.typec, self.interner)
    }

    fn ensure_dest(
        &mut self,
        dest: VRef<ValueMir>,
        view: &FuncMirView,
        layout: Layout,
    ) -> Result<IValue, StepError> {
        if let Some(dest) = self.current.unpack_slot(dest) {
            return Ok(dest);
        }

        let value = if layout.size > 8 || view.values[dest].referenced() {
            let ss = self.ctx.create_stack_slot(layout)?;
            IValue::Memory(ss)
        } else {
            IValue::Register(0)
        };

        let dest = self.current.root_value(dest);
        self.current.values[dest] = Some(ISlot::Value(value));
        Ok(value)
    }

    fn dereference(
        &mut self,
        target: VRef<ValueMir>,
        ret: VRef<ValueMir>,
        view: &FuncMirView,
    ) -> Option<ISlot> {
        let target = self.current.unpack_slot(target).unwrap();
        let ty = self.current.value_ty(ret, view);
        let layout = self.layout(ty);
        if layout.is_zero_sized() {
            return None;
        }

        Some(ISlot::Value(match target {
            IValue::Register(target) => IValue::Memory(target as *mut u8),
            mem => mem,
        }))
    }

    fn reference(
        &mut self,
        target: VRef<ValueMir>,
        ret: VRef<ValueMir>,
        view: &FuncMirView,
    ) -> Option<ISlot> {
        let target = self.current.unpack_slot(target).unwrap();
        let ty = self.current.value_ty(ret, view);
        let layout = self.layout(ty);
        if layout.is_zero_sized() {
            return None;
        }

        Some(ISlot::Value(match target {
            IValue::Memory(target) => IValue::Register(target as i64),
            mem => mem,
        }))
    }

    fn field(
        &mut self,
        target: VRef<ValueMir>,
        field: u32,
        func: &FuncMirView,
    ) -> (Option<ISlot>, u32) {
        let ty = self.current.value_ty(target, func);
        let layout = self.layout(ty);
        let offset =
            self.layouts.offsets[layout.offsets][field as usize] + self.current.offsets[target];
        (self.current.values[target], offset)
    }
}

pub struct PreparedInterpreterCtx<'a>(&'a mut InterpreterCtx);

impl Deref for PreparedInterpreterCtx<'_> {
    type Target = InterpreterCtx;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl DerefMut for PreparedInterpreterCtx<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

#[derive(Default)]
pub struct InterpreterCtx {
    stack: Vec<u8>,
    frames: Vec<PushedFrame>,
    fuel: usize,
}

impl InterpreterCtx {
    pub fn prepare(&mut self, stack_size: usize, fuel: usize) -> PreparedInterpreterCtx {
        self.stack.clear();
        self.stack.reserve(stack_size);
        self.frames.clear();
        self.fuel = fuel;
        PreparedInterpreterCtx(self)
    }

    fn create_stack_slot(&mut self, layout: Layout) -> Result<*mut u8, StepError> {
        unsafe {
            let current = self.stack.as_mut_ptr().add(self.stack.len());
            let padding = current.align_offset(layout.align.get() as usize);
            let add = layout.size as usize + padding;
            if self.stack.len() + add > self.stack.capacity() {
                return Err(StepError::Inerpreter(InterpreterError::StackOverflow));
            }
            self.stack.set_len(self.stack.len() + add);
            Ok(current.add(padding))
        }
    }
}

struct PushedFrame {
    frame: StackFrame,
    return_value: VRef<ValueMir>,
}

pub struct StackFrame {
    pub params: Vec<Ty>,
    pub block: VRef<BlockMir>,
    pub values: ShadowMap<ValueMir, Option<ISlot>>,
    pub offsets: ShadowMap<ValueMir, u32>,
    pub types: PushMap<MirTy>,
    pub instr: u32,
    pub frame_base: u32,
    pub func: FuncMir,
}

impl StackFrame {
    fn load_register(&self, reg: VRef<ValueMir>) -> i64 {
        match self.unpack_slot(reg) {
            Some(IValue::Register(reg)) => reg,
            _ => unreachable!(),
        }
    }

    fn unpack_slot(&self, reg: VRef<ValueMir>) -> Option<IValue> {
        self.unpack_slot_low(self.values[reg]?)
    }

    fn unpack_slot_low(&self, reg: ISlot) -> Option<IValue> {
        match reg {
            ISlot::Variable(reg) => self.unpack_slot(reg),
            ISlot::Value(value) => Some(value),
        }
    }

    fn root_value(&self, reg: VRef<ValueMir>) -> VRef<ValueMir> {
        match self.values[reg] {
            Some(ISlot::Variable(reg)) => self.root_value(reg),
            _ => reg,
        }
    }

    fn value_ty(&self, value: VRef<ValueMir>, view: &FuncMirView) -> Ty {
        self.types[view.values[value].ty()].ty
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ISlot {
    Variable(VRef<ValueMir>),
    Value(IValue),
}

#[derive(Clone, Copy, Debug)]
pub enum IValue {
    Register(IRegister),
    Memory(*mut u8),
}

impl IValue {}

unsafe impl Send for IValue {}

#[derive(Debug)]
pub enum InterpreterError {
    OutOfFuel,
    StackOverflow,
    MissingBody,
}

enum StepError {
    Return(Option<ISlot>),
    Call,
    Inerpreter(InterpreterError),
}
