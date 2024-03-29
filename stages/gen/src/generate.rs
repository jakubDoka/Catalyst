use core::slice;
use std::{array, cmp::Ordering};

use cranelift_codegen::ir::{
    self,
    condcodes::{FloatCC, IntCC},
    InstBuilder, MemFlags, Type,
};
use mir::*;
use resources::Resources;
use storage::*;

use type_creator::type_creator;
use types::*;

use crate::{
    ctx::{CompileRequestView, ComputedValue},
    //interpreter::{ISlot, IValue},
    *,
};

pub mod function_loading;
pub mod size_calc;

pub struct Generator<'ctx> {
    pub layouts: &'ctx mut GenLayouts,
    pub gen: &'ctx mut Gen,
    pub gen_resources: &'ctx mut GenResources,
    pub interner: &'ctx mut Interner,
    pub types: &'ctx mut Types,
    pub request: CompileRequestView<'ctx>,
    pub resources: &'ctx Resources,
}

impl Generator<'_> {
    pub fn generate(
        &mut self,
        signature: Signature,
        ret: VRef<ValueMir>,
        args: &[VRef<ValueMir>],
        params: &[Ty],
        root: VRef<BlockMir>,
        mut builder: GenBuilder,
    ) {
        builder.func.clear();
        self.gen_resources.clear();

        let mut pass_signature = PassSignature::default();
        let has_s_ret = self.populate_signature(
            signature,
            params,
            &mut builder.inner.func.signature,
            &mut pass_signature,
            builder.isa,
        );
        builder.ret_pass_mode = pass_signature.ret;

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.append_block_params_for_function_params(entry_block);
        let mut params = builder.block_params(entry_block).to_bumpvec().into_iter();

        if has_s_ret {
            let addr = params.next().unwrap();
            self.gen_resources.values[ret] = GenValue {
                computed: Some(ComputedValue::Value(addr)),
                offset: 0,
                must_load: true,
            };
        }

        for ((arg, layout), pass) in args
            .iter()
            .copied()
            .map(|arg| (arg, self.ty_layout(builder.value_ty(arg))))
            .filter(|&(.., layout)| !layout.is_zero_sized())
            .collect::<BumpVec<_>>()
            .into_iter()
            .zip(pass_signature.args.drain(..))
        {
            let (value, must_load) = self
                .load_abi_value(&mut builder, arg, pass, &mut params)
                .expect("there should be enough parameters");

            let (value, must_load) = if builder.body.values[arg].referenced() && let ComputedValue::Value(v) = value {
                let ss = builder.create_stack_slot(layout);
                builder.ins().stack_store(v, ss, 0);
                (ComputedValue::StackSlot(ss), true)
            } else if builder.body.values[arg].mutable() && let ComputedValue::Value(v) = value {
                let var = builder.declare_var(v, arg);
                (ComputedValue::Variable(var), must_load)
            } else {
                (value, must_load)
            };
            self.gen_resources.values[arg] = GenValue {
                computed: Some(value),
                offset: 0,
                must_load,
            };
        }

        self.block(root, entry_block, true, &mut builder, true);
        while let Some((block, seal, ir_block)) = self.gen_resources.block_stack.pop() {
            self.block(block, ir_block, seal, &mut builder, false);
        }

        builder.finalize();
    }

    fn block(
        &mut self,
        block: VRef<BlockMir>,
        mut ir_block: ir::Block,
        seal: bool,
        builder: &mut GenBuilder,
        initial: bool,
    ) -> ir::Block {
        let BlockMir {
            insts,
            control_flow,
            passed,
            ..
        } = builder.body.blocks[block];

        if !initial {
            builder.switch_to_block(ir_block);
        }

        if let Some(passed) = passed
            && !self.gen_resources.values[passed].must_load
            && let layout = self.ty_layout(builder.value_ty(passed))
            && layout.size != 0
        {
            self.save_value(
                passed,
                builder.append_block_param(ir_block, layout.repr),
                0,
                false,
                builder,
            );
        }

        for &inst in &builder.body.insts[insts] {
            self.inst(inst, builder, &mut ir_block);
        }

        self.control_flow(control_flow, builder);

        if seal {
            builder.seal_block(ir_block);
        }

        ir_block
    }

    fn inst(&mut self, inst: InstMir, builder: &mut GenBuilder, dest_block: &mut ir::Block) {
        match inst {
            InstMir::Int(value, ret) => {
                let ty = self.ty_repr(builder.value_ty(ret));
                let value = builder.ins().iconst(ty, value);
                self.save_value(ret, value, 0, false, builder);
            }
            InstMir::Float(value, ret) => {
                let ty = self.ty_repr(builder.value_ty(ret));
                let value = match ty {
                    ir::types::F64 => builder.ins().f64const(value),
                    _ => builder.ins().f32const(value as f32),
                };
                self.save_value(ret, value, 0, false, builder);
            }
            InstMir::Assign(target, ret) => {
                self.assign_value(ret, target, builder);
            }
            InstMir::ConstAccess(r#const, ret) => self.const_access(r#const, ret, builder),
            InstMir::Call(call, ret) => self.call(call, ret, builder),
            InstMir::Ctor(fields, ret) => self.constructor(fields, ret, builder),
            InstMir::Deref(target, ret) => self.deref(target, ret, builder),
            InstMir::Ref(target, ret) => self.r#ref(target, ret, builder),
            InstMir::Field(header, field, ret) => self.field(header, field, ret, builder),
            InstMir::Var(value, ret) => {
                self.assign_value(ret, value, builder);
            }
            InstMir::Drop(drop) => self.drop(drop, builder, dest_block),
        };
    }

    fn const_access(
        &mut self,
        r#const: FragRef<Const>,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let layout = self.ty_layout(builder.value_ty(ret));
        if layout.is_zero_sized() {
            return;
        }
        let bits = self.types[r#const].value.as_register().unwrap();
        let value = match layout.repr {
            ir::types::I8 | ir::types::I16 | ir::types::I32 | ir::types::I64 => {
                builder.ins().iconst(layout.repr, bits as i64)
            }
            ir::types::F32 => builder.ins().f32const(f32::from_bits(bits as u32)),
            ir::types::F64 => builder.ins().f64const(f64::from_bits(bits)),
            _ => unimplemented!(),
        };

        self.save_value(ret, value, 0, false, builder);
    }

    fn drop(&mut self, drop: VRef<DropMir>, builder: &mut GenBuilder, dest_block: &mut ir::Block) {
        // we use tuples so that we can easily wrap the iterator items
        enum DropFrame {
            Drop {
                offset: u32,
                ty: Ty,
                dyn_offset: Option<ir::Value>,
            },
            Check {
                flag: ir::Value,
                cond: usize,
                next: ir::Block,
            },
            Goto {
                dest: ir::Block,
                next: ir::Block,
            },
            Loop {
                counter: ir::Value,
                max: usize,
                repeat: ir::Block,
                br: ir::Block,
            },
        }

        let value = builder.body.drops[drop].value;
        let ty = builder.value_ty(value);
        let mut funcs = self.request.children[self.request.drops[drop.index()]]
            .iter()
            .copied();
        let GenValue {
            mut computed,
            offset,
            must_load,
            ..
        } = self.gen_resources.values[value];

        if let Some(ComputedValue::Variable(var)) = computed && !must_load {
            computed = Some(ComputedValue::Value(builder.use_var(var)));
        }
        if let Some(ComputedValue::Value(val)) = computed && !must_load {
            let ss = builder.create_stack_slot(self.ty_layout(ty));
            builder.ins().stack_store(val, ss, 0);
            computed = Some(ComputedValue::StackSlot(ss));
        }
        let value = self.ref_low(computed, ty, offset, builder);

        // [BE INFORMED]: we push everything in reverse order so that we
        // iterate in chronological order
        let mut frontier = bumpvec![DropFrame::Drop {
            offset: 0,
            ty,
            dyn_offset: None
        }];
        while let Some(node) = frontier.pop() {
            let (offset, ty, dyn_offset) = match node {
                DropFrame::Drop {
                    offset,
                    ty,
                    dyn_offset,
                } => (offset, ty, dyn_offset),
                DropFrame::Check {
                    flag: cond,
                    cond: flag,
                    next: dest,
                } => {
                    let equal = builder.ins().icmp_imm(IntCC::Equal, cond, flag as i64);
                    let cond_block = builder.create_block();
                    builder.ins().brif(equal, cond_block, &[], dest, &[]);
                    let current_block = builder.current_block().expect("seems impossible");
                    builder.seal_block(current_block);
                    builder.switch_to_block(cond_block);
                    continue;
                }
                DropFrame::Goto { dest, next } => {
                    builder.ins().jump(dest, &[]);
                    let current_block = builder.current_block().expect("seems impossible");
                    builder.seal_block(current_block);
                    builder.switch_to_block(next);
                    if dest == next {
                        *dest_block = next;
                    }
                    continue;
                }
                DropFrame::Loop {
                    counter,
                    max,
                    repeat,
                    br,
                } => {
                    let finished = builder.ins().icmp_imm(IntCC::Equal, counter, max as i64);
                    builder.ins().brif(finished, br, &[], repeat, &[counter]);
                    builder.seal_block(repeat);
                    builder.switch_to_block(br);
                    continue;
                }
            };

            if !type_creator!(self).may_need_drop(ty) {
                continue;
            }

            // we can pass empty generics since all types are concrete
            if let Some(Some(..)) = type_creator!(self).is_drop(ty, &[]) {
                let CompileRequestChild { id, params, .. } = funcs.next().unwrap();
                let params = self.request.types[params].iter().copied();
                let (func, ..) = self.import_compiled_func(id, params, builder);
                let value = match offset == 0 {
                    true => value,
                    false => builder.ins().iadd_imm(value, offset as i64),
                };
                let value = match dyn_offset {
                    Some(offset) => builder.ins().iadd(value, offset),
                    None => value,
                };
                builder.ins().call(func, &[value]);
            }

            let layout = self.ty_layout(ty);

            let mut enum_expander = |s: &mut Self, params: FragSlice<Ty>, e: FragRef<Enum>| {
                let variants = s.types[e].variants;
                let flag_ty = s.types.enum_flag_ty(e);
                let flag_repr = s.ty_repr(Ty::Builtin(flag_ty));
                // in the future, flag offset can vary between enums
                let [flag_offset, value_offset] = [0, layout.align.get() as u32];

                let dest = builder.create_block();
                let mut current = Some(dest);
                let mut flag_value = None;
                for (cond, variant) in variants.keys().enumerate().rev() {
                    let ty = s.types[variant].ty;
                    let ty = type_creator!(s).instantiate(ty, params);
                    if !type_creator!(s).may_need_drop(ty) {
                        continue;
                    }

                    let next = current.take().unwrap_or_else(|| builder.create_block());

                    let &mut flag = flag_value.get_or_insert_with(|| {
                        builder.ins().load(
                            flag_repr,
                            MemFlags::new(),
                            value,
                            (offset + flag_offset) as i32,
                        )
                    });

                    frontier.push(DropFrame::Goto { dest, next });
                    frontier.push(DropFrame::Drop {
                        offset: offset + value_offset,
                        ty,
                        dyn_offset: None,
                    });
                    frontier.push(DropFrame::Check { flag, cond, next });
                }
            };

            match ty.to_base_and_params(self.types) {
                Ok((base, params)) => match base {
                    BaseTy::Struct(s) => {
                        layout
                            .offsets(&self.layouts.offsets)
                            .map(|of| of + offset)
                            .rev()
                            .zip(type_creator!(self).instantiate_fields(s, params))
                            .map(|(offset, ty)| DropFrame::Drop {
                                offset,
                                ty,
                                dyn_offset: None,
                            })
                            .collect_into(&mut *frontier);
                    }
                    BaseTy::Enum(e) => enum_expander(self, params, e),
                },
                Err(NonBaseTy::Array(a)) => {
                    let loop_counter_init = builder.ins().iconst(ir::types::I32, 0);
                    let loop_block = builder.create_block();
                    let loop_counter = builder.append_block_param(loop_block, ir::types::I32);
                    let exit_block = builder.create_block();

                    builder.ins().jump(loop_block, &[loop_counter_init]);
                    let current_block = builder.current_block().expect("seems impossible");
                    builder.seal_block(current_block);
                    builder.switch_to_block(loop_block);

                    let loop_counter = builder.ins().iadd_imm(loop_counter, 1);

                    frontier.push(DropFrame::Loop {
                        counter: loop_counter,
                        max: self.types[a].len as usize,
                        repeat: loop_block,
                        br: exit_block,
                    });
                    frontier.push(DropFrame::Drop {
                        dyn_offset: Some(loop_counter),
                        offset,
                        ty: self.types[a].item,
                    });
                }
                Err(..) => (),
            }
        }
    }

    fn control_flow(&mut self, control_flow: ControlFlowMir, builder: &mut GenBuilder) {
        match control_flow {
            ControlFlowMir::Return(ret) => {
                let mut ir_ret = self
                    .load_value(ret, builder, builder.ret_pass_mode)
                    .into_iter()
                    .flatten()
                    .collect::<BumpVec<_>>();
                if let Some(PassMode::Indirect(..)) = builder.ret_pass_mode {
                    ir_ret.clear();
                }
                builder.ins().return_(&ir_ret);
            }
            ControlFlowMir::Terminal => {
                builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
            }
            ControlFlowMir::Split {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.load_value(cond, builder, None)[0].unwrap();
                self.instantiate_block([then, otherwise], builder, |[then, otherwise], builder| {
                    builder.ins().brif(cond, then, &[], otherwise, &[])
                });
            }
            ControlFlowMir::Goto { dest, ret } => {
                let ret = ret
                    .filter(|&ret| !self.gen_resources.values[ret].must_load)
                    .and_then(|ret| self.load_value(ret, builder, None)[0]);
                let args = ret.as_ref().map(slice::from_ref).unwrap_or_default();
                self.instantiate_block([dest], builder, |[b], builder| builder.ins().jump(b, args));
            }
        }
    }

    fn deref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        if let GenValue {
            computed: Some(..),
            must_load: true,
            ..
        } = self.gen_resources.values[ret]
        {
            let value = self.gen_resources.values[target].computed.unwrap();
            self.save_value(ret, value, 0, true, builder);
            return;
        }

        let Some(value) = self.load_value(target, builder, None)[0] else { return; };
        self.gen_resources.values[ret] = GenValue {
            must_load: true,
            computed: Some(ComputedValue::Value(value)),
            ..self.gen_resources.values[target]
        };
    }

    fn r#ref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];
        assert!(must_load || computed.is_none());
        let ty = builder.value_ty(target);
        let addr = self.ref_low(computed, ty, offset, builder);
        self.save_value(ret, addr, 0, false, builder);
    }

    fn ref_low(
        &mut self,
        computed: Option<ComputedValue>,
        ty: Ty,
        offset: i32,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let ptr_ty = builder.ptr_ty();
        let value = match computed {
            Some(ComputedValue::Value(value)) => value,
            Some(ComputedValue::Variable(var)) => builder.use_var(var),
            Some(ComputedValue::StackSlot(ss)) => {
                return builder.ins().stack_addr(ptr_ty, ss, offset)
            }
            None => {
                let align = self.ty_layout(ty).align;
                return builder.ins().iconst(ptr_ty, align.get() as i64);
            }
        };

        match offset == 0 {
            true => value,
            false => builder.ins().iadd_imm(value, offset as i64),
        }
    }

    fn field(
        &mut self,
        header: VRef<ValueMir>,
        field: u32,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        // field just changes offset
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[header];
        let header_ty = builder.value_ty(header);
        let field_offset = self
            .ty_layout(header_ty)
            .offsets(&self.layouts.offsets)
            .nth(field as usize)
            .unwrap();
        if self.gen_resources.values[ret].computed.is_some() {
            self.save_value(
                ret,
                computed.unwrap(),
                offset + field_offset as i32,
                must_load,
                builder,
            );
        } else {
            self.gen_resources.values[ret] = GenValue {
                computed,
                offset: offset + field_offset as i32,
                must_load,
            };
        }
    }

    fn constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let layout = self.ty_layout(builder.value_ty(ret));

        self.ensure_target_low(ret, None, true, builder);

        let base_value = self.gen_resources.values[ret];

        layout
            .offsets(&self.layouts.offsets)
            .zip(&builder.body.value_args[fields])
            .for_each(|(offset, &field)| {
                self.gen_resources.values[field] = GenValue {
                    offset: offset as i32 + base_value.offset,
                    ..base_value
                };
            });
    }

    fn call(&mut self, call: VRef<CallMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let CallMir { args, .. } = builder.body.calls[call];
        let CompileRequestChild { id, func, params } = self.request.calls[call.index()];

        if self.types[func].flags.contains(FuncFlags::BUILTIN) {
            if func == Func::CAST {
                self.cast(args, ret, builder)
            } else if func == Func::SIZEOF {
                self.sizeof(self.request.types[params][0], ret, builder)
            } else {
                let args = builder.body.value_args[args]
                    .iter()
                    .filter_map(|&arg| self.load_value(arg, builder, None)[0])
                    .collect::<BumpVec<_>>();
                self.builtin_call(func, args, ret, builder);
            }

            return;
        }

        let params = self.request.types[params].iter().copied();
        let (func, struct_ret, pass_sig) = self.import_compiled_func(id, params, builder);

        let struct_ptr = struct_ret.then(|| {
            if self.gen_resources.values[ret].computed.is_some() {
                self.load_value(ret, builder, None)[0].unwrap()
            } else {
                let ptr_ty = builder.ptr_ty();
                let layout = self.ty_layout(builder.value_ty(ret));
                let stack_slot = builder.create_stack_slot(layout);
                self.gen_resources.values[ret] = GenValue {
                    computed: Some(ComputedValue::StackSlot(stack_slot)),
                    offset: 0,
                    must_load: true,
                };
                builder.ins().stack_addr(ptr_ty, stack_slot, 0)
            }
        });

        let args = struct_ptr
            .into_iter()
            .chain(
                builder.body.value_args[args]
                    .iter()
                    .zip(&pass_sig.args)
                    .flat_map(|(&arg, &pass)| self.load_value(arg, builder, Some(pass)))
                    .flatten(),
            )
            .collect::<BumpVec<_>>();

        let inst = builder.ins().call(func, &args);
        if !struct_ret && let Some(pass) = pass_sig.ret {
            let params = builder.inst_results(inst).to_bumpvec();
            let (computed, must_load) = self.load_abi_value(builder, ret, pass, params.into_iter())
                .expect("unsufficient param count");
            self.save_value(ret, computed, 0, must_load, builder);
        }
    }

    fn load_abi_value(
        &mut self,
        builder: &mut GenBuilder,
        value: VRef<ValueMir>,
        mode: PassMode,
        mut params: impl Iterator<Item = ir::Value>,
    ) -> Option<(ComputedValue, bool)> {
        let layout = self.ty_layout(builder.value_ty(value));
        let (computed, must_load) = match mode {
            PassMode::Pair(a, ..) => {
                // for now this is simpler
                let ss = if let Some(ComputedValue::StackSlot(ss)) =
                    self.gen_resources.values[value].computed
                {
                    ss
                } else {
                    builder.create_stack_slot(layout)
                };
                let av = params.next()?;
                builder.ins().stack_store(av, ss, 0);
                let bv = params.next()?;
                builder.ins().stack_store(bv, ss, a.bytes() as i32);
                (ComputedValue::StackSlot(ss), true)
            }
            PassMode::Single(t, ..) => {
                let arg = params.next()?;
                let casted = match t != layout.repr {
                    true => builder.ins().bitcast(layout.repr, MemFlags::new(), arg),
                    false => arg,
                };
                (ComputedValue::Value(casted), false)
            }
            PassMode::Indirect(..) => (ComputedValue::Value(params.next()?), true),
        };
        Some((computed, must_load))
    }

    fn cast(&mut self, args: VRefSlice<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let [value] = builder.body.value_args[args] else {
            unreachable!()
        };
        self.assign_value(ret, value, builder)
    }

    fn sizeof(&mut self, ty: Ty, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let ptr_ty = builder.ptr_ty();
        let ty = self.ty_layout(ty);
        let value = builder.ins().iconst(ptr_ty, ty.size as i64);
        self.save_value(ret, value, 0, false, builder);
    }

    fn builtin_call(
        &mut self,
        func_id: FragRef<Func>,
        args: BumpVec<ir::Value>,
        target: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let Func {
            signature, name, ..
        } = self.types[func_id];
        let ret_ty = self.ty_repr(signature.ret);
        let op_str = name
            .get(self.interner)
            .split_whitespace()
            .nth(1)
            .unwrap_or(name.get(self.interner));
        let signed = signature.ret.is_signed();
        let is_bool = signature.ret == Ty::BOOL;

        macro_rules! helper {
            (floats) => {
                ir::types::F32 | ir::types::F64
            };
            (ints) => {
                ir::types::I8 | ir::types::I16 | ir::types::I32 | ir::types::I64
            };
            (sints) => {
                helper!(ints)
            };
            (uints) => {
                helper!(ints)
            };
            (binary) => {
                helper!(ints)
            };
            (scalars) => {
                helper!(binary) | helper!(floats)
            };

            (dispatch ($a:expr, $b:expr) {$(
                ($group:tt $($ops:tt: $func:tt),* $(,)?),
            )*}) => {
                match (builder.func.dfg.value_type($a), op_str, signed) {
                    $(
                        $((helper!($group), helper!(@op $ops), helper!(@sign $group)) => helper!(@func $func, $a, $b),)*
                    )*
                    val => unimplemented!("{:?}", val),
                }
            };

            (@op $str:literal) => {$str};
            (@op $tt:tt) => {stringify!($tt)};

            (@func $name:ident, $a:expr, $b:expr) => {builder.ins().$name($a, $b)};
            (@func (icmp $cmp:ident), $a:expr, $b:expr) => {builder.ins().icmp(IntCC::$cmp, $a, $b)};
            (@func (fcmp $cmp:ident), $a:expr, $b:expr) => {builder.ins().fcmp(FloatCC::$cmp, $a, $b)};

            (@sign uints) => {false};
            (@sign sints) => {true};
            (@sign $any:tt) => {_};
        }

        let value = match *args.as_slice() {
            [a, b] => helper!(dispatch (a, b) {
                (ints
                    +: iadd, "-": isub, *: imul, <<: ishl,
                    ==: (icmp Equal),
                    !=: (icmp NotEqual)),
                (uints
                    /: udiv, %: urem, >>: ushr,
                    >: (icmp UnsignedGreaterThan),
                    <: (icmp UnsignedLessThan),
                    >=: (icmp UnsignedGreaterThanOrEqual),
                    <=: (icmp UnsignedLessThanOrEqual)),
                (sints
                    /: sdiv, %: srem, >>: sshr,
                    >: (icmp SignedGreaterThan),
                    <: (icmp SignedLessThan),
                    >=: (icmp SignedGreaterThanOrEqual),
                    <=: (icmp SignedLessThanOrEqual)),
                (binary
                    &: band, |: bor, ^: bxor),
                (floats
                    +: fadd, "-": fsub, *: fmul, /: fdiv,
                    ==: (fcmp Equal),
                    !=: (fcmp NotEqual),
                    >: (fcmp GreaterThan),
                    <: (fcmp LessThan),
                    >=: (fcmp GreaterThanOrEqual),
                    <=: (fcmp LessThanOrEqual)),
            }),
            [v] => match (builder.func.dfg.value_type(v), ret_ty) {
                (from @ helper!(ints), to @ helper!(ints)) => {
                    let v = match is_bool {
                        true => builder.ins().icmp_imm(IntCC::NotEqual, v, 0),
                        false => v,
                    };
                    self.convert_int(builder, signature, from, to, v)
                }
                (from @ helper!(floats), to @ helper!(floats)) => {
                    match from.bytes().cmp(&to.bytes()) {
                        Ordering::Less => builder.ins().fpromote(to, v),
                        Ordering::Equal => v,
                        Ordering::Greater => builder.ins().fdemote(to, v),
                    }
                }
                (helper!(floats), to @ helper!(ints)) => match signed {
                    true => builder.ins().fcvt_to_sint_sat(to, v),
                    false => builder.ins().fcvt_to_uint_sat(to, v),
                },
                (helper!(ints), to @ helper!(floats)) => {
                    match self.types[signature.args][0].is_signed() {
                        true => builder.ins().fcvt_from_sint(to, v),
                        false => builder.ins().fcvt_from_uint(to, v),
                    }
                }
                others => unimplemented!("{others:?}"),
            },
            ref slice => unimplemented!("{slice:?}"),
        };

        self.save_value(target, value, 0, false, builder);
    }

    fn convert_int(
        &self,
        builder: &mut GenBuilder,
        signature: Signature,
        from: Type,
        to: Type,
        v: ir::Value,
    ) -> ir::Value {
        match from.bytes().cmp(&to.bytes()) {
            Ordering::Greater => builder.ins().ireduce(to, v),
            Ordering::Equal => v,
            Ordering::Less => {
                let arg_is_signed = self.types[signature.args][0].is_signed();
                match arg_is_signed && signature.ret.is_signed() {
                    true => builder.ins().sextend(to, v),
                    false => builder.ins().uextend(to, v),
                }
            }
        }
    }

    fn instantiate_block<W, const BLOCK_COUNT: usize>(
        &mut self,
        blocks: [VRef<BlockMir>; BLOCK_COUNT],
        builder: &mut GenBuilder,
        access: impl FnOnce([ir::Block; BLOCK_COUNT], &mut GenBuilder) -> W,
    ) {
        let gen_blocks =
            self.gen_resources
                .blocks
                .get_array(blocks)
                .zip(blocks)
                .map(|(slot, block)| {
                    slot.get_or_insert_with(|| GenBlock {
                        id: builder.create_block(),
                        forward_visit_count: builder.body.blocks[block].ref_count
                            - builder.body.blocks[block].cycles,
                        backward_visit_count: builder.body.blocks[block].cycles,
                    })
                });

        let ids = array::from_fn(|i| gen_blocks[i].id);
        access(ids, builder);

        for (gen_block, block) in gen_blocks.zip(blocks) {
            if gen_block.forward_visit_count == 0 {
                if gen_block.backward_visit_count != 1 {
                    gen_block.backward_visit_count -= 1;
                } else {
                    builder.seal_block(gen_block.id);
                }
            } else {
                if gen_block.forward_visit_count == 1 {
                    self.gen_resources.block_stack.push((
                        block,
                        gen_block.backward_visit_count == 0,
                        gen_block.id,
                    ));
                }
                gen_block.forward_visit_count -= 1;
            }
        }
    }

    fn assign_value(
        &mut self,
        target: VRef<ValueMir>,
        source: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        if !self.is_representable(source, builder) {
            return;
        }
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[source];
        self.save_value(
            target,
            computed.expect("value must be computed by now"),
            offset,
            must_load,
            builder,
        );
    }

    fn save_value(
        &mut self,
        target: VRef<ValueMir>,
        source_value: impl Into<ComputedValue>,
        source_offset: i32,
        must_load_source: bool,
        builder: &mut GenBuilder,
    ) {
        let source_value = source_value.into();

        let ptr_ty = builder.ptr_ty();
        let layout = self.ty_layout(builder.value_ty(target));

        if layout.is_zero_sized() {
            return;
        }

        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];

        let must_load_target = match computed.is_none() {
            true => builder.body.values[target].referenced() || layout.on_stack(),
            false => must_load,
        };

        if must_load_source && must_load_target {
            let target_value = self
                .ensure_target(target, None, builder)
                .expect("impossible");

            let mut get_addr = |value, offset: i32| match value {
                ComputedValue::StackSlot(slot) => builder.ins().stack_addr(ptr_ty, slot, offset),
                ComputedValue::Value(val) => match offset != 0 {
                    true => builder.ins().iadd_imm(val, offset as i64),
                    false => val,
                },
                ComputedValue::Variable(var) => builder.use_var(var),
            };
            let target_addr = get_addr(target_value, offset);
            let source_addr = get_addr(source_value, source_offset);

            let non_overlapping = matches!((target_value, source_value), (
                ComputedValue::StackSlot(a),
                ComputedValue::StackSlot(b),
            ) if a != b);

            let config = builder.isa.frontend_config();
            builder.emit_small_memory_copy(
                config,
                target_addr,
                source_addr,
                layout.size as u64,
                layout.align.get(),
                layout.align.get(),
                non_overlapping,
                MemFlags::new(),
            );

            return;
        }

        let (source, ..) = Self::load_value_low(
            builder,
            must_load_source,
            layout,
            source_offset,
            source_value,
            PassMode::Single(layout.repr, ir::ArgumentExtension::None),
        );

        let Some(target_value) = self.ensure_target(target, Some(source), builder) else {
            return;
        };

        if must_load_target {
            match target_value {
                ComputedValue::Value(val) => {
                    builder.ins().store(MemFlags::new(), source, val, offset);
                }
                ComputedValue::Variable(var) => {
                    let val = builder.use_var(var);
                    builder.ins().store(MemFlags::new(), source, val, offset);
                }
                ComputedValue::StackSlot(ss) => {
                    builder.ins().stack_store(source, ss, offset);
                }
            }
        } else {
            match target_value {
                ComputedValue::Value(value) => {
                    let new_value = self.set_bit_field(source, value, offset, builder);
                    self.gen_resources.values[target].computed =
                        Some(ComputedValue::Value(new_value));
                }
                ComputedValue::StackSlot(..) => unreachable!(),
                ComputedValue::Variable(var) => {
                    let value = builder.use_var(var);
                    let new_value = self.set_bit_field(source, value, offset, builder);
                    builder.def_var(var, new_value);
                }
            }
        }
    }

    fn load_value(
        &mut self,
        target: VRef<ValueMir>,
        builder: &mut GenBuilder,
        pass_mode: Option<PassMode>,
    ) -> [Option<ir::Value>; 2] {
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];
        let layout = self.ty_layout(builder.value_ty(target));
        if layout.is_zero_sized() {
            return [None; 2];
        }
        let pass_mode =
            pass_mode.unwrap_or(PassMode::Single(layout.repr, ir::ArgumentExtension::None));
        let (a, b) = Self::load_value_low(
            builder,
            must_load,
            layout,
            offset,
            computed.expect("value should be computed"),
            pass_mode,
        );

        [Some(a), b]
    }

    fn load_value_low(
        builder: &mut GenBuilder,
        must_load_source: bool,
        layout: Layout,
        source_offset: i32,
        source_value: ComputedValue,
        pass_mode: PassMode,
    ) -> (ir::Value, Option<ir::Value>) {
        let source = if must_load_source {
            if layout.on_stack() {
                'a: {
                    let value = match source_value {
                        ComputedValue::Value(val) => val,
                        ComputedValue::Variable(var) => builder.use_var(var),
                        ComputedValue::StackSlot(ss) => {
                            break 'a builder.ins().stack_addr(layout.repr, ss, source_offset);
                        }
                    };

                    match source_offset == 0 {
                        true => value,
                        false => builder.ins().iadd_imm(value, source_offset as i64),
                    }
                }
            } else {
                match source_value {
                    ComputedValue::Value(val) => {
                        builder
                            .ins()
                            .load(layout.repr, MemFlags::new(), val, source_offset)
                    }
                    ComputedValue::Variable(var) => {
                        let val = builder.use_var(var);
                        builder
                            .ins()
                            .load(layout.repr, MemFlags::new(), val, source_offset)
                    }
                    ComputedValue::StackSlot(ss) => {
                        builder.ins().stack_load(layout.repr, ss, source_offset)
                    }
                }
            }
        } else {
            let value = match source_value {
                ComputedValue::Value(val) => val,
                ComputedValue::Variable(var) => builder.use_var(var),
                ComputedValue::StackSlot(..) => unreachable!(),
            };

            let shifted = match source_offset.cmp(&0) {
                Ordering::Less => builder.ins().ishl_imm(value, -source_offset as i64 * 8),
                Ordering::Equal => value,
                Ordering::Greater => builder.ins().ushr_imm(value, source_offset as i64 * 8),
            };

            let resized = match builder.func.dfg.value_type(shifted).bytes() > layout.size {
                true => builder.ins().ireduce(layout.repr.as_int(), shifted),
                false => shifted,
            };

            match builder.func.dfg.value_type(resized) != layout.repr {
                true => {
                    assert_ne!(
                        layout.repr.as_int(),
                        layout.repr,
                        "{:?} {}",
                        builder.func.dfg.value_type(resized),
                        builder.func.display(),
                    );
                    builder.ins().bitcast(layout.repr, MemFlags::new(), resized)
                }
                false => resized,
            }
        };

        match pass_mode {
            PassMode::Pair(a, b) => {
                let av = builder.ins().load(a, MemFlags::new(), source, 0);
                let bv = builder
                    .ins()
                    .load(b, MemFlags::new(), source, a.bytes() as i32);
                (av, Some(bv))
            }
            PassMode::Single(ty, ..) => match layout.repr != ty {
                true => (builder.ins().bitcast(ty, MemFlags::new(), source), None),
                false => (source, None),
            },
            PassMode::Indirect(..) => (source, None),
        }
    }

    fn ensure_target(
        &mut self,
        target: VRef<ValueMir>,
        source_value: Option<ir::Value>,
        builder: &mut GenBuilder,
    ) -> Option<ComputedValue> {
        self.ensure_target_low(target, source_value, false, builder)
    }

    fn ensure_target_low(
        &mut self,
        target: VRef<ValueMir>,
        source_value: Option<ir::Value>,
        force_mutable: bool,
        builder: &mut GenBuilder,
    ) -> Option<ComputedValue> {
        if let value @ Some(..) = self.gen_resources.values[target].computed {
            return value;
        }

        let referenced = builder.body.values[target].referenced();

        let layout = self.ty_layout(builder.value_ty(target));
        let must_load = layout.on_stack() || referenced;
        let computed = if must_load {
            let ss = builder.create_stack_slot(layout);
            if let Some(source) = source_value {
                builder.ins().stack_store(source, ss, 0);
            }
            ComputedValue::StackSlot(ss)
        } else {
            let init = source_value.unwrap_or_else(|| builder.ins().iconst(layout.repr, 0));
            if builder.body.values[target].mutable()
                || force_mutable
                || builder.body.values[target].var()
            {
                ComputedValue::Variable(builder.declare_var(init, target))
            } else {
                ComputedValue::Value(init)
            }
        };

        self.gen_resources.values[target] = GenValue {
            computed: Some(computed),
            offset: 0,
            must_load,
        };

        source_value.is_none().then_some(computed)
    }

    fn set_bit_field(
        &mut self,
        source: ir::Value,
        target: ir::Value,
        target_offset: i32,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let source_size = builder.func.dfg.value_type(source).bytes();
        let source_repr = builder.func.dfg.value_type(source);
        let target_repr = builder.func.dfg.value_type(target);
        let casted = match source_repr.as_int() != source_repr {
            true => builder
                .ins()
                .bitcast(source_repr.as_int(), MemFlags::new(), source),
            false => source,
        };

        let balanced = match target_repr.bytes() > source_size {
            true => builder.ins().uextend(target_repr, casted),
            false => return casted,
        };

        let shifted = match target_offset.cmp(&0) {
            Ordering::Less => builder.ins().ushr_imm(balanced, -target_offset as i64 * 8),
            Ordering::Equal => balanced,
            Ordering::Greater => builder.ins().ishl_imm(balanced, target_offset as i64 * 8),
        };

        let insert_mask = !((
            // results into source size full of ones
            (1 << (source_size as i64 * 8)) - 1
        ) << (target_offset as i64 * 8));
        let target = builder.ins().band_imm(target, insert_mask);
        builder.ins().bor(target, shifted)
    }
}
