use core::fmt;
use core::fmt::Write;

use storage::*;
use types::*;

pub trait TypeDisplay {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result;
}

pub fn display<T: TypeDisplay>(types: &Types, interner: &Interner, value: T) -> String {
    let mut str = String::new();
    value.display(types, interner, &mut str).unwrap();
    str
}

pub fn display_list<T: TypeDisplay>(
    types: &Types,
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

    first.display(types, interner, out)?;
    for value in values {
        out.push(sep);
        value.display(types, interner, out)?;
    }

    out.push(close);
    Ok(())
}

impl TypeDisplay for Spec {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result {
        match self {
            Spec::Base(base) => out.push_str(types[base].name.get(interner)),
            Spec::Instance(instance) => types[instance].display(types, interner, out)?,
        }

        Ok(())
    }
}

impl TypeDisplay for SpecInstance {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result {
        display_spec_instance(types, interner, self.base, &types[self.args], out)
    }
}

pub fn display_spec_instance(
    types: &Types,
    interner: &Interner,
    base: FragRef<SpecBase>,
    args: &[Ty],
    to: &mut String,
) -> fmt::Result {
    to.push_str(types[base].name.get(interner));
    display_list(types, interner, args.iter().copied(), to, ['[', ',', ']'])
}

pub fn display_spec_sum(
    types: &Types,
    interner: &Interner,
    spec: impl Iterator<Item = Spec>,
    to: &mut String,
) -> fmt::Result {
    display_list(types, interner, spec, to, [':', '+', ' '])
}

impl TypeDisplay for Func {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result {
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
            types,
            interner,
            types[upper_generics]
                .iter()
                .chain(&types[generics])
                .copied(),
            out,
            ['[', ',', ']'],
        )?;
        out.push(' ');
        out.push_str(name.get(interner));
        display_list(
            types,
            interner,
            types[signature.args].iter().copied(),
            out,
            ['(', ',', ')'],
        )?;
        out.push_str(" -> ");
        signature.ret.display(types, interner, out)
    }
}

impl TypeDisplay for FragSlice<Spec> {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result {
        display_spec_sum(types, interner, types[self].iter().copied(), out)
    }
}

pub fn display_func_name(
    types: &Types,
    interner: &Interner,
    func: FragRef<Func>,
    to: &mut String,
) -> fmt::Result {
    let Func {
        name, loc, owner, ..
    } = types[func];
    write!(to, "{}\\", loc.source().index()).unwrap();
    if let Some(owner) = owner {
        owner.base(types).display(types, interner, to)?;
        write!(to, "\\").unwrap();
    }
    to.push_str(name.get(interner));
    Ok(())
}

impl TypeDisplay for Ty {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result {
        match self {
            Ty::Struct(r#struct) => {
                write!(out, "{}\\", types[r#struct].loc.source().index())?;
                out.push_str(types[r#struct].name.get(interner))
            }
            Ty::Enum(r#enum) => {
                write!(out, "{}\\", types[r#enum].loc.source().index())?;
                out.push_str(types[r#enum].name.get(interner))
            }
            Ty::Instance(instance) => types[instance].display(types, interner, out)?,
            Ty::Pointer(ptr) => {
                let base = types[ptr.ty()];
                out.push('^');
                write!(out, "{}", ptr.mutability.to_mutability())?;
                base.display(types, interner, out)?;
            }
            Ty::Array(array) => {
                let Array { item, len, .. } = types[array];
                display_array(types, interner, item, len, out)?;
            }
            Ty::Param(i) => write!(out, "param{i}")?,
            Ty::Builtin(b) => out.push_str(b.name()),
        }
        Ok(())
    }
}

pub fn display_array(
    types: &Types,
    interner: &Interner,
    item: Ty,
    len: ArraySize,
    to: &mut String,
) -> fmt::Result {
    write!(to, "[")?;
    item.display(types, interner, to)?;
    write!(to, "; {}]", len)?;
    Ok(())
}

impl TypeDisplay for Instance {
    fn display(self, types: &Types, interner: &Interner, out: &mut String) -> fmt::Result {
        display_instance(types, interner, self.base, &types[self.args], out)
    }
}

pub fn display_instance(
    types: &Types,
    interner: &Interner,
    base: GenericTy,
    args: &[Ty],
    out: &mut String,
) -> fmt::Result {
    base.as_ty().display(types, interner, out).unwrap();
    display_list(types, interner, args.iter().copied(), out, ['[', ',', ']'])
}

pub fn type_diff(types: &Types, interner: &Interner, pattern: Ty, value: Ty) -> String {
    let mut buffer = String::new();
    type_diff_recurse(types, interner, pattern, value, &mut buffer).unwrap();
    buffer
}

fn type_diff_recurse(
    types: &Types,
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
            type_diff_recurse(types, interner, types[pattern.ty()], types[value.ty()], to)?;
        }
        (Ty::Instance(pattern), Ty::Instance(value)) => {
            type_diff_recurse(
                types,
                interner,
                types[pattern].base.as_ty(),
                types[value].base.as_ty(),
                to,
            )?;
            let Some((&pattern_first, pattern_others)) = types[types[pattern].args].split_first() else {
                return Ok(());
            };
            let Some((&value_first, value_others)) = types[types[value].args].split_first() else {
                return Ok(());
            };

            to.push('[');
            type_diff_recurse(types, interner, pattern_first, value_first, to)?;
            for (&pattern, &value) in pattern_others.iter().zip(value_others) {
                to.push_str(", ");
                type_diff_recurse(types, interner, pattern, value, to)?;
            }
            to.push(']');
        }
        _ => pattern.display(types, interner, to)?,
    }

    Ok(())
}

pub fn display_bin_op(
    types: &Types,
    interner: &Interner,
    op: Ident,
    lhs: Ty,
    rhs: Ty,
    to: &mut String,
) -> fmt::Result {
    lhs.display(types, interner, to)?;
    to.push(' ');
    to.push_str(op.get(interner));
    to.push(' ');
    rhs.display(types, interner, to)
}
