// The classification code for the x86_64 ABI is taken from the clay language
// https://github.com/jckarter/clay/blob/master/compiler/src/externals.cpp

use cranelift_codegen::ir::{self, ArgumentExtension, Type};
use storage::*;
use types::*;
use typec_u::type_creator;

use crate::*;

/// Classification of "eightbyte" components.
// N.B., the order of the variants is from general to specific,
// such that `unify(a, b)` is the "smaller" of `a` and `b`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Class {
    Int,
    Sse,
    SseUp,
}

#[derive(Clone, Copy, Debug)]
struct Memory;

// Currently supported vector size (AVX-512).
const LARGEST_VECTOR_SIZE: usize = 512;
const MAX_EIGHTBYTES: usize = LARGEST_VECTOR_SIZE / 64;

fn classify_arg_low(
    generator: &mut Generator,
    ty: Ty,
    params: &[Ty],
    classes: &mut [Option<Class>],
    offset: u32,
) -> Result<(), Memory> {
    let layout = generator.ty_layout(ty);
    if offset & (layout.align.get() as u32 - 1) != 0 {
        return layout.is_zero_sized().then_some(()).ok_or(Memory);
    }

    use types::Builtin::*;
    let mut c = match ty {
        Ty::Builtin(bt) => match bt {
            Bool | Char | U8 | U16 | U32 | Uint | Short | Long | LongLong | Cint => Class::Int,
            F32 | F64 => Class::Sse,
            Unit | Terminal | Mutable | Immutable => return Ok(()),
        },
        Ty::Struct(s) => return classify_struct_arg(generator, s, params, layout, classes, offset),
        Ty::Enum(e) => return classify_enum_arg(generator, e, layout, params, classes, offset),
        Ty::Instance(i) => {
            return classify_instance_arg(generator, i, params, layout, classes, offset)
        }
        Ty::Pointer(..) => Class::Int,
        Ty::Param(index) => {
            return classify_arg_low(generator, params[index as usize], &[], classes, offset);
        }
        Ty::Array(_) => todo!(),
    };

    // Fill in `cls` for scalars (Int/Sse) and vectors (Sse).
    let align = 8;
    let first = (offset / align) as usize;
    let last = ((offset + layout.size - 1) / align) as usize;
    for cls in &mut classes[first..=last] {
        *cls = Some(cls.map_or(c, |old| old.min(c)));

        // Everything after the first Sse "eightbyte"
        // component is the upper half of a register.
        if c == Class::Sse {
            c = Class::SseUp;
        }
    }

    Ok(())
}

fn classify_enum_arg(
    generator: &mut Generator,
    e: FragRef<Enum>,
    layout: Layout,
    params: &[Ty],
    classes: &mut [Option<Class>],
    offset: u32,
) -> Result<(), Memory> {
    let value_offset = 1;
    let flag_ty = Ty::Builtin(generator.types.enum_flag_ty(e));
    classify_arg_low(generator, flag_ty, params, classes, offset)?;
    let offset = layout
        .offsets(&generator.layouts.offsets)
        .nth(value_offset)
        .unwrap()
        + offset;
    for ty in type_creator!(generator).instantiate_variants(e, params) {
        classify_arg_low(generator, ty, params, classes, offset)?;
    }
    Ok(())
}

fn classify_struct_arg(
    generator: &mut Generator,
    s: FragRef<Struct>,
    params: &[Ty],
    layout: Layout,
    classes: &mut [Option<Class>],
    offset: u32,
) -> Result<(), Memory> {
    for (ty, field_offset) in type_creator!(generator)
        .instantiate_fields(s, params)
        .into_iter()
        .zip(
            layout
                .offsets(&generator.layouts.offsets)
                .collect::<BumpVec<_>>(),
        )
    {
        classify_arg_low(generator, ty, params, classes, offset + field_offset)?;
    }
    Ok(())
}

fn classify_instance_arg(
    generator: &mut Generator,
    instance: FragRef<Instance>,
    params: &[Ty],
    layout: Layout,
    classes: &mut [Option<Class>],
    offset: u32,
) -> Result<(), Memory> {
    let Instance { base, args } = generator.types[instance];
    let params = type_creator!(generator).instantiate_slice(args, params);
    match base {
        GenericTy::Struct(s) => classify_struct_arg(generator, s, &params, layout, classes, offset),
        GenericTy::Enum(e) => classify_enum_arg(generator, e, layout, &params, classes, offset),
    }
}

fn classify_arg(
    generator: &mut Generator,
    ty: Ty,
    params: &[Ty],
) -> Result<[Option<Class>; MAX_EIGHTBYTES], Memory> {
    let layout = generator.ty_layout(ty);
    let n = layout.size.div_ceil(8) as usize;
    if n > MAX_EIGHTBYTES {
        return Err(Memory);
    }

    let mut classes = [None; MAX_EIGHTBYTES];
    classify_arg_low(generator, ty, params, &mut classes, 0)?;
    if n > 2 {
        if classes[0] != Some(Class::Sse) {
            return Err(Memory);
        }
        if classes[1..n].iter().any(|&c| c != Some(Class::SseUp)) {
            return Err(Memory);
        }
    } else {
        let mut i = 0;
        while i < n {
            if classes[i] == Some(Class::SseUp) {
                classes[i] = Some(Class::Sse);
            } else if classes[i] == Some(Class::Sse) {
                i += 1;
                while i != n && classes[i] == Some(Class::SseUp) {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
    }

    Ok(classes)
}

fn reg_component(
    generator: &GenLayouts,
    cls: &[Option<Class>],
    i: &mut usize,
    size: u32,
) -> Option<Type> {
    if *i >= cls.len() {
        return None;
    }

    match cls[*i] {
        None => None,
        Some(Class::Int) => {
            *i += 1;
            Some(generator.repr_for_size(size).0)
        }
        Some(Class::Sse) => {
            let vec_len = 1 + cls[*i + 1..]
                .iter()
                .take_while(|&&c| c == Some(Class::SseUp))
                .count();
            *i += vec_len;
            Some(if vec_len == 1 {
                match size {
                    4 => ir::types::F32,
                    _ => ir::types::F64,
                }
            } else {
                ir::types::I8.by(size).unwrap()
            })
        }
        Some(c) => unreachable!("reg_component: unhandled class {:?}", c),
    }
}

fn cast_target(generator: &mut GenLayouts, classes: &[Option<Class>], size: u32) -> PassMode {
    let mut i = 0;
    let lo = reg_component(generator, classes, &mut i, size).unwrap();
    let offset = 8 * (i as u32);
    let mut target = PassMode::Single(lo, ArgumentExtension::None);
    if size > offset {
        if let Some(hi) = reg_component(generator, classes, &mut i, size - offset) {
            target = PassMode::Pair(lo, hi);
        }
    }
    assert_eq!(reg_component(generator, classes, &mut i, 0), None);
    target
}

const MAX_INT_REGS: usize = 6; // RDI, RSI, RDX, RCX, R8, R9
const MAX_SSE_REGS: usize = 8; // XMM0-7

pub fn compute_abi_info(
    generator: &mut Generator,
    sig: Signature,
    params: &[Ty],
    target: &mut PassSignature,
) {
    let mut int_regs = MAX_INT_REGS;
    let mut sse_regs = MAX_SSE_REGS;

    let mut arg_or_ret = |generator: &mut Generator, arg: Ty, is_arg: bool| {
        let arg = type_creator!(generator).instantiate(arg, params);
        let mut cls_or_mem = classify_arg(generator, arg, params);
        let layout = generator.ty_layout(arg);

        if layout.is_zero_sized() {
            return None;
        }

        if is_arg {
            if let Ok(cls) = cls_or_mem {
                let mut needed_int = 0;
                let mut needed_sse = 0;
                for c in cls {
                    match c {
                        Some(Class::Int) => needed_int += 1,
                        Some(Class::Sse) => needed_sse += 1,
                        _ => {}
                    }
                }
                match (
                    int_regs.checked_sub(needed_int),
                    sse_regs.checked_sub(needed_sse),
                ) {
                    (Some(left_int), Some(left_sse)) => {
                        int_regs = left_int;
                        sse_regs = left_sse;
                    }
                    _ if arg.is_aggregate() => cls_or_mem = Err(Memory),
                    _ => (),
                }
            }
        }

        Some(match cls_or_mem {
            Err(Memory) => {
                if !is_arg {
                    assert_eq!(int_regs, MAX_INT_REGS);
                    int_regs -= 1;
                }
                PassMode::Indirect(generator.layouts.ptr_ty, layout.size)
            }
            Ok(ref cls) => {
                // split into sized chunks passed individually
                if arg.is_aggregate() {
                    let size = layout.size;
                    cast_target(generator.layouts, cls, size)
                } else {
                    PassMode::Single(layout.repr, super::extension_for(arg))
                }
            }
        })
    };

    target.ret = arg_or_ret(generator, sig.ret, false);
    target.args.clear();
    generator.types[sig.args]
        .to_bumpvec()
        .into_iter()
        .filter_map(|arg| arg_or_ret(generator, arg, true))
        .collect_into(&mut target.args);
}
