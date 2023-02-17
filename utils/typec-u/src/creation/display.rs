use core::fmt;
use core::fmt::Write;

use storage::*;
use typec_t::*;

pub trait TypeDisplay {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result;
}

pub fn display<T: TypeDisplay>(typec: &Typec, interner: &Interner, value: T) -> String {
    let mut str = String::new();
    value.display(typec, interner, &mut str).unwrap();
    str
}

pub fn display_list<T: TypeDisplay>(
    typec: &Typec,
    interner: &Interner,
    values: impl IntoIterator<Item = T>,
    out: &mut String,
    [open, sep, close]: [char; 3],
) -> fmt::Result {
    out.push(open);

    let mut values = values.into_iter();
    let Some(first) = values.next() else {
        out.push(close);
        return Ok(());
    };

    first.display(typec, interner, out)?;
    for value in values {
        out.push(sep);
        value.display(typec, interner, out)?;
    }

    out.push(close);
    Ok(())
}

impl TypeDisplay for Spec {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result {
        match self {
            Spec::Base(base) => out.push_str(typec[base].name.get(interner)),
            Spec::Instance(instance) => typec[instance].display(typec, interner, out)?,
        }

        Ok(())
    }
}

impl TypeDisplay for SpecInstance {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result {
        display_spec_instance(typec, interner, self.base, &typec[self.args], out)
    }
}

pub fn display_spec_instance(
    typec: &Typec,
    interner: &Interner,
    base: FragRef<SpecBase>,
    args: &[Ty],
    to: &mut String,
) -> fmt::Result {
    to.push_str(typec[base].name.get(interner));
    display_list(typec, interner, args.iter().copied(), to, ['[', ',', ']'])
}

pub fn display_spec_sum(
    typec: &Typec,
    interner: &Interner,
    spec: impl Iterator<Item = Spec>,
    to: &mut String,
) -> fmt::Result {
    display_list(typec, interner, spec, to, [':', '+', ' '])
}

impl TypeDisplay for Func {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result {
        let Func {
            signature,
            generics,
            upper_generics,
            name,
            ..
        } = self;
        out.push_str("fn ");
        signature
            .cc
            .as_ref()
            .map(|cc| cc.get(interner))
            .map_or(Ok(()), |cc| write!(out, "\"{}\" ", cc))?;
        display_list(
            typec,
            interner,
            typec[upper_generics]
                .iter()
                .chain(&typec[generics])
                .copied(),
            out,
            ['[', ',', ']'],
        )?;
        out.push(' ');
        out.push_str(name.get(interner));
        display_list(
            typec,
            interner,
            typec[signature.args].iter().copied(),
            out,
            ['(', ',', ')'],
        )?;
        out.push_str(" -> ");
        signature.ret.display(typec, interner, out)
    }
}

impl TypeDisplay for FragSlice<Spec> {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result {
        display_spec_sum(typec, interner, typec[self].iter().copied(), out)
    }
}

pub fn display_func_name(
    typec: &Typec,
    interner: &Interner,
    func: FragRef<Func>,
    to: &mut String,
) -> fmt::Result {
    let Func {
        name, loc, owner, ..
    } = typec[func];
    if let Some(loc) = loc {
        write!(to, "{}\\", loc.module.index()).unwrap();
    }
    if let Some(owner) = owner {
        owner.base(typec).display(typec, interner, to)?;
        write!(to, "\\").unwrap();
    }
    to.push_str(name.get(interner));
    Ok(())
}

impl TypeDisplay for Ty {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result {
        match self {
            Ty::Struct(r#struct) => {
                write!(
                    out,
                    "{}\\",
                    typec[r#struct]
                        .loc
                        .map_or(usize::MAX, |loc| loc.module.index())
                )?;
                out.push_str(typec[r#struct].name.get(interner))
            }
            Ty::Enum(r#enum) => {
                write!(
                    out,
                    "{}\\",
                    typec[r#enum]
                        .loc
                        .map_or(usize::MAX, |loc| loc.module.index())
                )?;
                out.push_str(typec[r#enum].name.get(interner))
            }
            Ty::Instance(instance) => typec[instance].display(typec, interner, out)?,
            Ty::Pointer(ptr) => {
                let base = typec[ptr.ty()];
                out.push('^');
                write!(out, "{}", ptr.mutability.to_mutability())?;
                base.display(typec, interner, out)?;
            }
            Ty::Array(array) => {
                let Array { item, len, .. } = typec[array];
                display_array(typec, interner, item, len, out)?;
            }
            Ty::Param(i) => write!(out, "param{i}")?,
            Ty::Builtin(b) => out.push_str(b.name()),
        }
        Ok(())
    }
}

pub fn display_array(
    typec: &Typec,
    interner: &Interner,
    item: Ty,
    len: ArraySize,
    to: &mut String,
) -> fmt::Result {
    write!(to, "[")?;
    item.display(typec, interner, to)?;
    write!(to, "; {}]", len)?;
    Ok(())
}

impl TypeDisplay for Instance {
    fn display(self, typec: &Typec, interner: &Interner, out: &mut String) -> fmt::Result {
        display_instance(typec, interner, self.base, &typec[self.args], out)
    }
}

pub fn display_instance(
    typec: &Typec,
    interner: &Interner,
    base: GenericTy,
    args: &[Ty],
    out: &mut String,
) -> fmt::Result {
    base.as_ty().display(typec, interner, out).unwrap();
    display_list(typec, interner, args.iter().copied(), out, ['[', ',', ']'])
}

pub fn type_diff(typec: &Typec, interner: &Interner, pattern: Ty, value: Ty) -> String {
    let mut buffer = String::new();
    type_diff_recurse(typec, interner, pattern, value, &mut buffer).unwrap();
    buffer
}

fn type_diff_recurse(
    typec: &Typec,
    interner: &Interner,
    pattern: Ty,
    value: Ty,
    to: &mut String,
) -> fmt::Result {
    match (pattern, value) {
        _ if pattern == value => to.push('_'),
        (Ty::Pointer(pattern), Ty::Pointer(value)) => {
            to.push('^');
            write!(to, "{}", pattern.mutability.to_mutability()).unwrap();
            type_diff_recurse(typec, interner, typec[pattern.ty()], typec[value.ty()], to)?;
        }
        (Ty::Instance(pattern), Ty::Instance(value)) => {
            type_diff_recurse(
                typec,
                interner,
                typec[pattern].base.as_ty(),
                typec[value].base.as_ty(),
                to,
            )?;
            let Some((&pattern_first, pattern_others)) = typec[typec[pattern].args].split_first() else {
                return Ok(());
            };
            let Some((&value_first, value_others)) = typec[typec[value].args].split_first() else {
                return Ok(());
            };

            to.push('[');
            type_diff_recurse(typec, interner, pattern_first, value_first, to)?;
            for (&pattern, &value) in pattern_others.iter().zip(value_others) {
                to.push_str(", ");
                type_diff_recurse(typec, interner, pattern, value, to)?;
            }
            to.push(']');
        }
        _ => pattern.display(typec, interner, to)?,
    }

    Ok(())
}

pub fn display_bin_op(
    typec: &Typec,
    interner: &Interner,
    op: Ident,
    lhs: Ty,
    rhs: Ty,
    to: &mut String,
) -> fmt::Result {
    lhs.display(typec, interner, to)?;
    to.push_str(op.get(interner));
    rhs.display(typec, interner, to)
}
