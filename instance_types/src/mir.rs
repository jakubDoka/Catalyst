use std::{marker::PhantomData, ops::IndexMut};

use cranelift_codegen::{ir::Type, packed_option::PackedOption};
use cranelift_entity::{EntityRef, PrimaryMap};

use storage::*;
use typec_types::*;
use lexer::*;

use crate::*;

impl<E: EntityRef, N: LinkedNode<E>> LinkedList<E, N> for PrimaryMap<E, N> {}

#[macro_export]
macro_rules! impl_linked_node {
    ($entity:ty, $target:ty) => {
        impl $crate::mir::LinkedNode<$entity> for $target {
            fn prev(&self) -> Option<$entity> {
                self.prev.expand()
            }

            fn next(&self) -> Option<$entity> {
                self.next.expand()
            }
        }
    };

    (mut $entity:ty, $target:ty) => {
        impl $crate::mir::MutLinkNode<$entity> for $target {
            fn set_prev(&mut self, prev: Option<$entity>) {
                self.prev = prev.into();
            }

            fn set_next(&mut self, next: Option<$entity>) {
                self.next = next.into();
            }
        }
    };

    (inout $entity:ty, $target:ty) => {
        $crate::impl_linked_node!($entity, $target);
        $crate::impl_linked_node!(mut $entity, $target);
    };
}

pub trait LinkedNode<E: EntityRef>: 'static {
    fn prev(&self) -> Option<E>;
    fn next(&self) -> Option<E>;
}

pub trait MutLinkNode<E: EntityRef>: LinkedNode<E> {
    fn set_prev(&mut self, prev: Option<E>);
    fn set_next(&mut self, next: Option<E>);
}

pub trait LinkedList<E: EntityRef, N: LinkedNode<E>>: Sized + IndexMut<E, Output = N> {
    fn linked_iter(&self, root: Option<E>) -> LinkedIter<Self, E, N> {
        LinkedIter {
            storage: self,
            current: root,
            _ph: PhantomData,
        }
    }

    fn linked_iter_mut(&mut self, root: Option<E>) -> LinkedIterMut<Self, E, N> {
        LinkedIterMut {
            storage: self,
            current: root,
            _ph: PhantomData,
        }
    }
}

macro_rules! gen_iter {
    ($name:ident, $($mutability:tt)?) => {
        pub struct $name<'a, S, E, N> {
            storage: &'a $($mutability)? S,
            current: Option<E>,
            _ph: PhantomData<N>,
        }

        impl<'a, S: LinkedList<E, N>, E: EntityRef, N: LinkedNode<E>> Iterator for $name<'a, S, E, N> {
            type Item = (E, &'a $($mutability)? N);

            fn next(&mut self) -> Option<Self::Item> {
                self.current.map(|current_ent| {
                    let current = unsafe { std::mem::transmute::<_, &'a $($mutability)? N>(& $($mutability)? self.storage[current_ent]) };
                    self.current = current.next();
                    (current_ent, current)
                })
            }
        }
    };
}

gen_iter!(LinkedIter,);
gen_iter!(LinkedIterMut, mut);

#[derive(Default, Clone, Copy, Debug)]
pub struct BlockEnt {
    pub prev: PackedOption<Block>,
    pub next: PackedOption<Block>,
    pub start: PackedOption<Inst>,
    pub end: PackedOption<Inst>,
    pub params: ValueList,
}

crate::impl_linked_node!(inout Block, BlockEnt);
gen_entity!(Block);

#[derive(Debug, Default)]
pub struct InstEnt {
    pub prev: PackedOption<Inst>,
    pub next: PackedOption<Inst>,
    pub kind: InstKind,
    pub value: PackedOption<Value>,
}

impl InstEnt {
    pub fn new(kind: InstKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    pub fn value(self, value: impl Into<Option<Value>>) -> Self {
        Self {
            value: value.into().into(),
            ..self
        }
    }
}

#[derive(Debug)]
pub enum InstKind {
    IndirectCall(Value, ValueList),
    FuncPtr(Func),
    GlobalAccess(Global),
    BitCast(Value),
    DerefPtr(Value),
    TakePtr(Value),
    Offset(Value),
    StackAddr(StackSlot),
    Variable,
    Assign(Value),
    JumpIfFalse(Block),
    Jump(Block),
    Call(typec_types::Func, TyList, ValueList, Span),
    IntLit(u128),
    BoolLit(bool),
    Uninit,
    Return,
}

impl Default for InstKind {
    fn default() -> Self {
        InstKind::Return
    }
}

impl InstKind {
    pub fn is_terminating(&self) -> bool {
        matches!(self, Self::Return | Self::Jump(_))
    }
}

crate::impl_linked_node!(inout Inst, InstEnt);
gen_entity!(Inst);

#[derive(Debug, Clone, Copy)]
pub struct ValueEnt {
    pub ty: Ty,
    pub offset: Offset,
    pub flags: MirFlags,
}

impl ValueEnt {
    pub fn new(ty: Ty) -> ValueEnt {
        ValueEnt {
            ty,
            offset: Offset::ZERO,
            flags: MirFlags::empty(),
        }
    }

    pub fn offset(self, offset: Offset) -> Self {
        Self { offset, ..self }
    }

    pub fn flags(self, flags: MirFlags) -> Self {
        Self { flags, ..self }
    }
}

gen_entity!(Value);
gen_entity!(ValueList);

bitflags! {
    #[derive(Default)]
    pub struct MirFlags: u32 {
        /// The value is a pointer.
        const POINTER = 1 << 0;
        /// The value can be assigned to
        const ASSIGNABLE = 1 << 1;
    }
}

impl_bool_bit_and!(MirFlags);

#[derive(Debug, Clone, Copy)]
pub struct StackEnt {
    pub size: u32,
}

impl StackEnt {
    pub fn new(size: Offset, ptr_ty: Type) -> Self {
        Self {
            size: size.arch(ptr_ty.bytes() == 4) as u32,
        }
    }
}

gen_entity!(StackSlot);

impl MirDisplay<'_> {
    pub fn value_to_string(&self, value: Value) -> String {
        format!(
            "{}:{}>{}[{:?}]",
            value,
            self.func_ctx.values[value].offset.arch64,
            ty_display!(self, self.func_ctx.values[value].ty),
            self.func_ctx.values[value].flags,
        )
    }
}

impl std::fmt::Display for MirDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "(signature) {{")?;

        for (i, stack) in self.func_ctx.stacks.iter() {
            writeln!(f, "  {} = stack({})", i, stack.size)?;
        }

        for (id, block) in self
            .func_ctx
            .blocks
            .linked_iter(self.func_ctx.start.expand())
        {
            writeln!(
                f,
                "  {}({}): {{",
                id,
                self.func_ctx
                    .value_slices
                    .get(block.params)
                    .iter()
                    .map(|&v| self.value_to_string(v))
                    .collect::<Vec<_>>()
                    .join(", "),
            )?;

            for (_, inst) in self.func_ctx.insts.linked_iter(block.start.expand()) {
                match inst.kind {
                    InstKind::IndirectCall(func, values) => {
                        let args = self
                            .func_ctx
                            .value_slices
                            .get(values)
                            .iter()
                            .map(|&v| format!("{v}"))
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(
                            f,
                            "\t{} = func_ptr {} ({})",
                            self.value_to_string(inst.value.unwrap()),
                            func,
                            args,
                        )?;
                    }
                    InstKind::Uninit => {
                        writeln!(f, "\t{} = uninit", self.value_to_string(inst.value.unwrap()))?;
                    }
                    InstKind::FuncPtr(func) => {
                        writeln!(
                            f,
                            "\t{} = func_ptr {}",
                            self.value_to_string(inst.value.unwrap()),
                            func
                        )?;
                    }
                    InstKind::BitCast(value) => {
                        writeln!(
                            f,
                            "\t{} = bitcast {}",
                            self.value_to_string(inst.value.unwrap()),
                            value
                        )?;
                    }
                    InstKind::GlobalAccess(g) => {
                        writeln!(
                            f,
                            "\t{} = global {}",
                            self.value_to_string(inst.value.unwrap()),
                            g
                        )?;
                    }
                    InstKind::TakePtr(target) => {
                        writeln!(
                            f,
                            "\t{} = ptr {}",
                            self.value_to_string(inst.value.unwrap()),
                            target
                        )?;
                    }
                    InstKind::DerefPtr(target) => {
                        writeln!(
                            f,
                            "\t{} = deref {}",
                            self.value_to_string(inst.value.unwrap()),
                            target
                        )?;
                    }
                    InstKind::Offset(target) => {
                        writeln!(
                            f,
                            "\t{} = offset {}",
                            self.value_to_string(inst.value.unwrap()),
                            target
                        )?;
                    }
                    InstKind::StackAddr(stack) => {
                        writeln!(
                            f,
                            "\t{} = stack {}",
                            self.value_to_string(inst.value.unwrap()),
                            stack
                        )?;
                    }
                    InstKind::Variable => {
                        writeln!(f, "\tlet {}", inst.value.unwrap())?;
                    }
                    InstKind::Assign(value) => {
                        writeln!(f, "\t{} = {}", inst.value.unwrap(), value)?;
                    }
                    InstKind::JumpIfFalse(block) => {
                        writeln!(f, "\tif {} goto {}", inst.value.unwrap(), block)?;
                    }
                    InstKind::Jump(block) => {
                        if let Some(value) = inst.value.expand() {
                            writeln!(f, "\tgoto {} with {}", block, value)?;
                        } else {
                            writeln!(f, "\tgoto {}", block)?;
                        }
                    }
                    InstKind::Call(func, params, values, ..) => {
                        let args = self
                            .func_ctx
                            .value_slices
                            .get(values)
                            .iter()
                            .map(|&v| format!("{v}"))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let params = self
                            .ty_lists
                            .get(params)
                            .iter()
                            .map(|&v| format!("{}", ty_display!(self, v)))
                            .collect::<Vec<_>>()
                            .join(", ");
                        if let Some(value) = inst.value.expand() {
                            writeln!(
                                f,
                                "\t{} = call [{}] {}({})",
                                self.value_to_string(value),
                                params,
                                func,
                                args
                            )?;
                        } else {
                            writeln!(f, "\tcall [{}] {}({})", params, func, args)?;
                        }
                    }
                    InstKind::IntLit(value) => {
                        writeln!(
                            f,
                            "\t{} = {}",
                            self.value_to_string(inst.value.unwrap()),
                            value
                        )?;
                    }
                    InstKind::BoolLit(value) => {
                        writeln!(
                            f,
                            "\t{} = {}",
                            self.value_to_string(inst.value.unwrap()),
                            value
                        )?;
                    }
                    InstKind::Return => {
                        if let Some(value) = inst.value.expand() {
                            writeln!(f, "\treturn {}", value)?;
                        } else {
                            writeln!(f, "\treturn")?;
                        }
                    }
                }
            }

            writeln!(f, "  }}")?;
        }

        Ok(())
    }
}
