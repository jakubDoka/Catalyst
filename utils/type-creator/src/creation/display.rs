use core::fmt;
use core::fmt::Write;

use storage::*;
use types::*;

use crate::TypeCreator;

pub trait TypeDisplay {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result;
}

impl TypeCreator<'_, '_> {
    pub fn display_to_string<T: TypeDisplay>(&self, value: T) -> String {
        let mut str = String::new();
        value.display(self, &mut str).unwrap();
        str
    }

    pub fn intern_with<E>(&mut self, value: impl FnOnce(&mut Self, &mut String) -> E) -> Ident {
        self.interner.intern_with(|s, t| {
            value(
                &mut Self {
                    types: self.types,
                    interner: s,
                    arena: self.arena,
                },
                t,
            )
        })
    }

    pub fn display_to_interner<T: TypeDisplay>(&mut self, value: T) -> Ident {
        self.intern_with(|s, t| value.display(s, t))
    }

    pub fn display_list<T: TypeDisplay>(
        &self,
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

        first.display(self, out)?;
        for value in values {
            out.push(sep);
            value.display(self, out)?;
        }

        out.push(close);
        Ok(())
    }

    pub fn display_bin_op(&self, op: Ident, lhs: Ty, rhs: Ty, to: &mut String) -> fmt::Result {
        lhs.display(self, to)?;
        to.push(' ');
        to.push_str(op.get(self.interner));
        to.push(' ');
        rhs.display(self, to)
    }

    pub fn type_diff(&self, pattern: Ty, value: Ty) -> String {
        let mut buffer = String::new();
        self.type_diff_recurse(pattern, value, &mut buffer).unwrap();
        buffer
    }

    fn type_diff_recurse(&self, pattern: Ty, value: Ty, to: &mut String) -> fmt::Result {
        match (pattern, value) {
            _ if pattern == value => to.push('_'),
            (Ty::Pointer(pattern), Ty::Pointer(value)) => {
                to.push('^');
                write!(to, "{}", pattern.mutability).unwrap();
                self.type_diff_recurse(*pattern.ty, *value.ty, to)?;
            }
            (Ty::Instance(pattern), Ty::Instance(value)) => {
                self.type_diff_recurse(pattern.base.as_ty(), value.base.as_ty(), to)?;
                let Some((&pattern_first, pattern_others)) = pattern.args.split_first() else {
                    return Ok(());
                };
                let Some((&value_first, value_others)) = value.args.split_first() else {
                    return Ok(());
                };

                to.push('[');
                self.type_diff_recurse(pattern_first, value_first, to)?;
                for (&pattern, &value) in pattern_others.iter().zip(value_others) {
                    to.push_str(", ");
                    self.type_diff_recurse(pattern, value, to)?;
                }
                to.push(']');
            }
            _ => pattern.display(self, to)?,
        }

        Ok(())
    }

    pub fn display_func_name(&self, func: FragRef<Func>, to: &mut String) -> fmt::Result {
        let TypeCreator {
            types,
            interner,
            arena,
        } = self;
        let Func {
            name, loc, owner, ..
        } = types[func];
        write!(to, "{}\\", loc.source().index()).unwrap();
        if let Some(owner) = owner {
            Ty::load(owner, types, arena).display(self, to)?;
            write!(to, "\\").unwrap();
        }
        to.push_str(name.get(interner));
        Ok(())
    }
}

impl TypeDisplay for Spec<'_> {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        match self {
            Spec::Base(base) => out.push_str(creator.types[base].name.get(creator.interner)),
            Spec::Instance(instance) => instance.display(creator, out)?,
        }

        Ok(())
    }
}

impl<T: TypeDisplay> TypeDisplay for ExpInstance<'_, T> {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        let ExpInstance { base, args } = self;
        base.display(creator, out)?;
        creator.display_list(args.iter().cloned(), out, ['[', ',', ']'])
    }
}

impl TypeDisplay for FragRef<SpecBase> {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        let TypeCreator {
            types, interner, ..
        } = creator;
        write!(out, "{}\\", types[self].loc.source().index())?;
        out.push_str(types[self].name.get(interner));
        Ok(())
    }
}

impl TypeDisplay for Func {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        let Func {
            signature,
            generics,
            name,
            ..
        } = self;
        out.push_str("fn ");
        signature
            .cc
            .as_ref()
            .map(|cc| cc.get(creator.interner))
            .map_or(Ok(()), |cc| write!(out, "\"{cc}\" "))?;
        out.push(' ');
        out.push_str(name.get(creator.interner));
        creator.display_list(
            creator.load_ty_slice(signature.args).iter().copied(),
            out,
            ['(', ',', ')'],
        )?;
        out.push_str(" -> ");
        Ty::load(signature.ret, creator.types, creator.arena).display(creator, out);
        generics.display(creator, out)
    }
}

impl TypeDisplay for WhereClause {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        let Self { predicates, .. } = self;
        if predicates.is_empty() {
            return Ok(());
        }
        write!(out, " where ")?;
        creator.display_list(
            creator.types[predicates].iter().copied(),
            out,
            [' ', ',', ' '],
        )
    }
}

impl TypeDisplay for WherePredicate {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        let Self { ty, bounds } = self;
        Ty::load(ty, creator.types, creator.arena).display(creator, out)?;
        write!(out, ": ")?;
        creator.display_list(
            creator.load_spec_slice(bounds).iter().copied(),
            out,
            [' ', '+', ' '],
        )
    }
}

impl TypeDisplay for FragSlice<CompactSpec> {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        creator.load_spec_slice(self).display(creator, out)
    }
}

impl TypeDisplay for &[Spec<'_>] {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        creator.display_list(self.iter().copied(), out, [':', '+', ' '])
    }
}

impl TypeDisplay for BaseTy {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        let TypeCreator {
            types, interner, ..
        } = creator;
        match self {
            BaseTy::Struct(r#struct) => {
                write!(out, "{}\\", types[r#struct].loc.source().index())?;
                out.push_str(types[r#struct].name.get(interner))
            }
            BaseTy::Enum(r#enum) => {
                write!(out, "{}\\", types[r#enum].loc.source().index())?;
                out.push_str(types[r#enum].name.get(interner))
            }
        }
        Ok(())
    }
}

impl TypeDisplay for Ty<'_> {
    fn display(self, creator: &TypeCreator, out: &mut String) -> fmt::Result {
        match self {
            Ty::Base(base) => base.display(creator, out)?,
            Ty::Instance(instance) => instance.display(creator, out)?,
            Ty::Pointer(ptr) => {
                let base = ptr.ty;
                out.push('^');
                write!(out, "{}", ptr.mutability)?;
                base.display(creator, out)?;
            }
            Ty::Array(ExpArray { item, len }) => {
                write!(out, "[")?;
                item.display(creator, out)?;
                write!(out, ";{len}]")?;
            }
            Ty::Param(i) => {
                write!(
                    out,
                    "{}",
                    creator
                        .types
                        .generic_names
                        .get(i.index.get())
                        .map(|s| s.get(creator.interner))
                        .unwrap_or(&format!("${}", i.index.get()))
                )?;
                write!(out, "\\TODO-Asoc-Types")?;
            }
            Ty::Builtin(b) => out.push_str(b.name()),
        }
        Ok(())
    }
}
