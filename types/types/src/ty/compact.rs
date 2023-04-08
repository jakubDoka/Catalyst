use std::mem;

use rkyv::{Archive, Deserialize, Serialize};
use storage::{FragAddr, FragRef, NonMaxU64, Relocated};

use crate::{
    Array, AsocTy, BaseTy, Builtin, DropSpec, Enum, Instance, SpecBase, SpecInstance, Struct,
    TyParamIdx,
};

#[derive(Clone, Copy)]
pub struct CompactFlag(u8);

impl CompactFlag {
    const TAG_SIZE: usize = 3;
    const TAG_MASK: u8 = (1 << Self::TAG_SIZE) - 1;
    const ELSE_MASK: u8 = !Self::TAG_MASK;

    const BUILTIN: u8 = 0;
    const ARRAY: u8 = 1;
    const POINTER: u8 = 2;
    const STRUCT: u8 = 3;
    const ENUM: u8 = 4;
    const INSTANCE: u8 = 5;
    const PARAM: u8 = 6;

    pub fn new(expanded: ExpandedFlag) -> Self {
        match expanded {
            ExpandedFlag::Builtin => Self(Self::BUILTIN),
            ExpandedFlag::Array => Self(Self::ARRAY),
            ExpandedFlag::Pointer { depth } => Self(Self::POINTER | (depth << Self::TAG_SIZE)),
            ExpandedFlag::Struct => Self(Self::STRUCT),
            ExpandedFlag::Enum => Self(Self::ENUM),
            ExpandedFlag::Instance => Self(Self::INSTANCE),
            ExpandedFlag::Param { has_asoc } => {
                Self(Self::PARAM | (has_asoc as u8) << Self::TAG_SIZE)
            }
        }
    }

    pub fn expanded(self) -> ExpandedFlag {
        match self.0 & Self::TAG_MASK {
            Self::BUILTIN => ExpandedFlag::Builtin,
            Self::ARRAY => ExpandedFlag::Array,
            Self::POINTER => ExpandedFlag::Pointer {
                depth: (self.0 & Self::ELSE_MASK) >> Self::TAG_SIZE,
            },
            Self::STRUCT => ExpandedFlag::Struct,
            Self::ENUM => ExpandedFlag::Enum,
            Self::INSTANCE => ExpandedFlag::Instance,
            Self::PARAM => ExpandedFlag::Param {
                has_asoc: (self.0 & Self::ELSE_MASK) != 0,
            },
            _ => unreachable!(),
        }
    }
}

pub enum ExpandedFlag {
    Builtin,
    Array,
    Pointer { depth: u8 },
    Struct,
    Enum,
    Instance,
    Param { has_asoc: bool },
}

#[derive(
    Clone, Copy, Serialize, Deserialize, Archive, Hash, PartialEq, Eq, PartialOrd, Ord, Debug,
)]

pub struct CompactBaseTy {
    is_enum: bool,
    thread: u8,
    index: u32,
}

impl Relocated for CompactBaseTy {
    fn mark(&self, marker: &mut storage::FragRelocMarker) {
        self.expanded().mark(marker);
    }

    fn remap(&mut self, ctx: &storage::FragMarks) -> Option<()> {
        let mut expanded = self.expanded();
        expanded.remap(ctx)?;
        *self = Self::new(expanded);
        Some(())
    }
}

impl CompactBaseTy {
    pub fn new(expanded: BaseTy) -> Self {
        match expanded {
            BaseTy::Struct(s) => Self {
                is_enum: false,
                thread: s.addr().thread,
                index: s.addr().index,
            },
            BaseTy::Enum(e) => Self {
                is_enum: true,
                thread: e.addr().thread,
                index: e.addr().index,
            },
        }
    }

    pub fn expanded(self) -> BaseTy {
        let addr = FragAddr::new(self.index, self.thread);
        if self.is_enum {
            BaseTy::Enum(FragRef::new(addr))
        } else {
            BaseTy::Struct(FragRef::new(addr))
        }
    }
}

#[derive(
    Clone, Copy, Serialize, Deserialize, Archive, Hash, PartialEq, Eq, PartialOrd, Ord, Debug,
)]
#[repr(transparent)]
pub struct CompactTy {
    repr: NonMaxU64,
}

impl Default for CompactTy {
    fn default() -> Self {
        Self::new(ExpandedTy::Builtin(Builtin::Unit))
    }
}

impl CompactTy {
    pub fn new(expanded: ExpandedTy) -> Self {
        let repr = CompactTyRepr::new(expanded);
        Self {
            repr: unsafe { mem::transmute(repr) },
        }
    }

    pub fn expanded(self) -> ExpandedTy {
        let repr: CompactTyRepr = unsafe { mem::transmute(self.repr) };
        repr.expanded()
    }

    pub fn to_ty<'a>(self, types: &crate::Types, arena: &storage::ProxyArena<'a>) -> crate::Ty<'a> {
        crate::Ty::load(self, types, arena)
    }

    pub fn drop_spec(self, types: &crate::Types) -> DropSpec {
        match self.expanded() {
            ExpandedTy::Builtin(..) => DropSpec::Copy,
            ExpandedTy::Array(array) => {
                let Array { item, len } = types[array];
                match len == 0 {
                    true => DropSpec::Copy,
                    false => item.drop_spec(types),
                }
            }
            ExpandedTy::Pointer { .. } => DropSpec::Copy,
            ExpandedTy::Node(Node::Instance(b)) => b.drop_spec(types),
            ExpandedTy::Node(Node::Instance(i)) => types[i].base.expanded().drop_spec(types),
            ExpandedTy::Param { param, asoc } => DropSpec::Hibrid,
        }
    }
}

impl Relocated for CompactTy {
    fn mark(&self, marker: &mut storage::FragRelocMarker) {
        self.expanded().mark(marker)
    }

    fn remap(&mut self, ctx: &storage::FragMarks) -> Option<()> {
        let mut expanded = self.expanded();
        expanded.remap(ctx)?;
        *self = Self::new(expanded);
        Some(())
    }
}

pub enum ExpandedTy {
    Builtin(Builtin),
    Array(FragRef<Array>),
    Pointer {
        depth: u8,
        mutability: TyParamIdx,
        ty: FragRef<CompactTy>,
    },
    Base(BaseTy),
    Instance(FragRef<Instance>),
    Param {
        param: TyParamIdx,
        asoc: Option<FragRef<AsocTy>>,
    },
}

impl Relocated for ExpandedTy {
    fn mark(&self, marker: &mut storage::FragRelocMarker) {
        match self {
            Self::Builtin(_) => {}
            Self::Array(array) => array.mark(marker),
            Self::Pointer { ty, .. } => ty.mark(marker),
            Self::Base(base) => base.mark(marker),
            Self::Instance(i) => i.mark(marker),
            Self::Param { asoc, .. } => {
                if let Some(asoc) = asoc {
                    asoc.mark(marker)
                }
            }
        }
    }

    fn remap(&mut self, ctx: &storage::FragMarks) -> Option<()> {
        match self {
            Self::Builtin(_) => {}
            Self::Array(array) => array.remap(ctx)?,
            Self::Pointer { ty, .. } => ty.remap(ctx)?,
            Self::Base(base) => base.remap(ctx)?,
            Self::Instance(i) => i.remap(ctx)?,
            Self::Param { asoc, .. } => {
                if let Some(asoc) = asoc {
                    asoc.remap(ctx)?
                }
            }
        }
        Some(())
    }
}

#[derive(
    Clone, Copy, Serialize, Deserialize, Archive, Hash, PartialEq, Eq, PartialOrd, Ord, Debug,
)]

pub struct CompactSpec {
    is_instance: bool,
    thread: u8,
    index: u32,
}

impl CompactSpec {
    pub fn new(expanded: ExpandedSpec) -> Self {
        match expanded {
            ExpandedSpec::Base(base) => Self {
                is_instance: false,
                thread: base.addr().thread,
                index: base.addr().index,
            },
            ExpandedSpec::Instance(instance) => Self {
                is_instance: true,
                thread: instance.addr().thread,
                index: instance.addr().index,
            },
        }
    }

    pub fn expanded(self) -> ExpandedSpec {
        let addr = FragAddr::new(self.index, self.thread);
        if self.is_instance {
            ExpandedSpec::Instance(FragRef::new(addr))
        } else {
            ExpandedSpec::Base(FragRef::new(addr))
        }
    }
}

impl Relocated for CompactSpec {
    fn mark(&self, marker: &mut storage::FragRelocMarker) {
        self.expanded().mark(marker)
    }

    fn remap(&mut self, ctx: &storage::FragMarks) -> Option<()> {
        let mut expanded = self.expanded();
        expanded.remap(ctx)?;
        *self = Self::new(expanded);
        Some(())
    }
}

pub enum ExpandedSpec {
    Base(FragRef<SpecBase>),
    Instance(FragRef<SpecInstance>),
}

impl Relocated for ExpandedSpec {
    fn mark(&self, marker: &mut storage::FragRelocMarker) {
        match self {
            Self::Base(base) => base.mark(marker),
            Self::Instance(instance) => instance.mark(marker),
        }
    }

    fn remap(&mut self, ctx: &storage::FragMarks) -> Option<()> {
        match self {
            Self::Base(base) => base.remap(ctx)?,
            Self::Instance(instance) => instance.remap(ctx)?,
        }
        Some(())
    }
}

union CompactTyRepr {
    frag_ref: CompactFragRef,
    builtin: CompactBuiltin,
    param: CompactParam,
}

impl CompactTyRepr {
    fn new(expanded: ExpandedTy) -> Self {
        match expanded {
            ExpandedTy::Builtin(builtin) => Self {
                builtin: CompactBuiltin {
                    flag: CompactFlag::new(ExpandedFlag::Builtin),
                    inner: builtin,
                },
            },
            ExpandedTy::Array(array) => Self {
                frag_ref: CompactFragRef::new(ExpandedFlag::Array, array),
            },
            ExpandedTy::Pointer {
                depth,
                ty,
                mutability,
            } => Self {
                param: {
                    let addr = ty.addr();
                    CompactParam {
                        flag: CompactFlag::new(ExpandedFlag::Pointer { depth }),
                        thread: addr.thread,
                        param: mutability,
                        index: addr.index,
                    }
                },
            },
            ExpandedTy::Base(BaseTy::Struct(r#struct)) => Self {
                frag_ref: CompactFragRef::new(ExpandedFlag::Struct, r#struct),
            },
            ExpandedTy::Base(BaseTy::Enum(r#enum)) => Self {
                frag_ref: CompactFragRef::new(ExpandedFlag::Enum, r#enum),
            },
            ExpandedTy::Node(Node::Instance(instance)) => Self {
                frag_ref: CompactFragRef::new(ExpandedFlag::Instance, instance),
            },
            ExpandedTy::Param { param, asoc } => Self {
                param: {
                    let addr = asoc.map_or(FragAddr::new(0, 0), |asoc| asoc.addr());
                    CompactParam {
                        flag: CompactFlag::new(ExpandedFlag::Param {
                            has_asoc: asoc.is_some(),
                        }),
                        thread: addr.thread,
                        param,
                        index: addr.index,
                    }
                },
            },
        }
    }

    fn expanded(self) -> ExpandedTy {
        let repr = self; // TODO: rename to self and get rid of

        macro frag_ref($struct:ident) {
            unsafe { FragRef::new(FragAddr::new(repr.$struct.index, repr.$struct.thread)) }
        }

        let flag = unsafe { repr.frag_ref.flag.expanded() };
        match flag {
            ExpandedFlag::Builtin => ExpandedTy::Builtin(unsafe { repr.builtin.inner }),
            ExpandedFlag::Array => ExpandedTy::Array(frag_ref!(frag_ref)),
            ExpandedFlag::Pointer { depth } => ExpandedTy::Pointer {
                depth,
                ty: frag_ref!(frag_ref),
                mutability: unsafe { repr.param.param },
            },
            ExpandedFlag::Struct => ExpandedTy::Base(BaseTy::Struct(frag_ref!(frag_ref))),
            ExpandedFlag::Enum => ExpandedTy::Base(BaseTy::Enum(frag_ref!(frag_ref))),
            ExpandedFlag::Instance => ExpandedTy::Instance(frag_ref!(frag_ref)),
            ExpandedFlag::Param { has_asoc } => ExpandedTy::Param {
                param: unsafe { repr.param.param },
                asoc: if has_asoc {
                    Some(frag_ref!(param))
                } else {
                    None
                },
            },
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct CompactFragRef {
    flag: CompactFlag,
    thread: u8,
    unused: u16,
    index: u32,
}

impl CompactFragRef {
    fn new<T>(flag: ExpandedFlag, frag_ref: FragRef<T>) -> Self {
        Self {
            flag: CompactFlag::new(flag),
            thread: frag_ref.addr().thread,
            unused: 0,
            index: frag_ref.addr().index,
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct CompactBuiltin {
    flag: CompactFlag,
    inner: Builtin,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct CompactParam {
    flag: CompactFlag,
    thread: u8,
    param: TyParamIdx,
    index: u32,
}
