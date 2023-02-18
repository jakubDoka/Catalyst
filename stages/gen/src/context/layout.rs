use std::{
    iter::{Copied, StepBy},
    slice::Iter,
};

use typec_u::{type_creator, TypeCreator};

use super::*;

#[derive(Default)]
pub struct GenLayouts {
    pub mapping: Map<Ty, Layout>,
    pub offsets: PushMap<Offset>,
    pub ptr_ty: Type,
    pub data_model: Option<CDataModel>,
    pub subclear_items: Vec<Ty>,
}

impl GenLayouts {
    pub fn ty_layout(&mut self, ty: Ty, params: &[Ty], mut creator: TypeCreator) -> Layout {
        if let Some(&layout) = self.mapping.get(&ty) {
            return layout;
        }

        let cr = type_creator!(creator);
        let res = match ty {
            Ty::Struct(s) => self.struct_layout(s, params, cr),
            Ty::Pointer(..) => Layout::from_type(self.ptr_ty),
            Ty::Builtin(bt) => self.builtin_layout(bt),
            Ty::Param(index) => self.ty_layout(params[index as usize], &[], cr),
            Ty::Instance(inst) => self.instance_layout(inst, params, cr),
            Ty::Enum(ty) => self.enum_layout(ty, params, cr),
            Ty::Array(a) => self.array_layout(a, params, cr),
        };

        if creator.typec.contains_params(ty) {
            self.subclear_items.push(ty);
        }

        self.mapping.insert(ty, res);

        res
    }

    fn array_layout(
        &mut self,
        array: FragRef<Array>,
        params: &[Ty],
        mut creator: TypeCreator,
    ) -> Layout {
        let Array { len, item } = creator.typec[array];
        let elem_layout = self.ty_layout(item, params, type_creator!(creator));
        let size = elem_layout.size * len;

        let (repr, on_stack) = self.repr_for_size(size);

        Layout {
            size,
            offsets: VSlice::new(0..elem_layout.size as usize),
            align: elem_layout.align,
            repr,
            flags: LayoutFlags::ON_STACK & on_stack | LayoutFlags::ARRAY,
        }
    }

    fn enum_layout(
        &mut self,
        r#enum: FragRef<Enum>,
        params: &[Ty],
        mut creator: TypeCreator,
    ) -> Layout {
        let tag_size = self.builtin_layout(creator.typec.enum_flag_ty(r#enum)).size;
        let (base_size, base_align) = creator.typec[creator.typec[r#enum].variants]
            .to_bumpvec()
            .into_iter()
            .map(|variant| self.ty_layout(variant.ty, params, type_creator!(creator)))
            .map(|layout| (layout.size, layout.align.get()))
            .max()
            .unwrap_or((0, 1));

        let align = base_align.max(tag_size as u8);
        let size = Layout::aligned_to(tag_size + base_size, align as u32);
        let (repr, on_stack) = self.repr_for_size(size);

        Layout {
            size,
            offsets: VSlice::new(0..align as usize),
            align: align.try_into().unwrap(),
            repr,
            flags: LayoutFlags::ON_STACK & on_stack | LayoutFlags::ENUM,
        }
    }

    fn instance_layout(
        &mut self,
        inst: FragRef<Instance>,
        params: &[Ty],
        mut creator: TypeCreator,
    ) -> Layout {
        let Instance { base, args } = creator.typec[inst];
        // remap the instance parameters so we can compute the layout correctly
        let params = creator.instantiate_slice(args, params);
        match base {
            GenericTy::Struct(s) => self.struct_layout(s, &params, creator),
            GenericTy::Enum(e) => self.enum_layout(e, &params, creator),
        }
    }

    fn struct_layout(
        &mut self,
        r#struct: FragRef<Struct>,
        params: &[Ty],
        mut creator: TypeCreator,
    ) -> Layout {
        let Struct { fields, .. } = creator.typec[r#struct];
        let mut offsets = bumpvec![cap fields.len()];

        let layouts = creator.typec[fields]
            .to_bumpvec()
            .into_iter()
            .map(|field| self.ty_layout(field.ty, params, type_creator!(creator)));

        let mut align = 1;
        let mut size = 0;
        for layout in layouts {
            align = align.max(layout.align.get());

            offsets.push(size);

            let padding = (layout.align.get() - (size as u8 & (layout.align.get() - 1)))
                & (layout.align.get() - 1);
            size += padding as u32;
            size += layout.size;
        }

        let (repr, on_stack) = self.repr_for_size(size);

        Layout {
            repr,
            offsets: self.offsets.extend(offsets),
            align: align.try_into().unwrap(),
            size,
            flags: LayoutFlags::ON_STACK & on_stack,
        }
    }

    fn builtin_layout(&self, bt: Builtin) -> Layout {
        use Builtin::*;

        let repr = match bt {
            Unit | Mutable | Immutable | Terminal => return Layout::EMPTY,
            Uint => self.ptr_ty,
            Char | U32 => types::I32,
            U16 => types::I16,
            Bool | U8 => types::I8,
            F32 => types::F32,
            F64 => types::F64,
            Short | Cint | Long | LongLong => self.c_type_repr(bt),
        };

        Layout::from_type(repr)
    }

    fn c_type_repr(&self, bt: Builtin) -> Type {
        use Builtin::*;
        let data_model = self
            .data_model
            .expect("missing c data model on current target");
        let size = match bt {
            Short => data_model.short_size(),
            Cint => data_model.int_size(),
            Long => data_model.long_size(),
            LongLong => data_model.long_long_size(),
            _ => unreachable!(),
        };

        match size {
            target_lexicon::Size::U8 => types::I8,
            target_lexicon::Size::U16 => types::I16,
            target_lexicon::Size::U32 => types::I32,
            target_lexicon::Size::U64 => types::I64,
        }
    }

    pub fn repr_for_size(&self, size: u32) -> (Type, bool) {
        if size > self.ptr_ty.bytes() {
            return (self.ptr_ty, true);
        }

        let repr = match size {
            8.. => types::I64,
            4.. => types::I32,
            2.. => types::I16,
            0.. => types::I8,
        };

        (repr, false)
    }

    pub fn clear(&mut self, isa: &Isa) {
        self.mapping.clear();
        self.offsets.clear();
        self.ptr_ty = isa.pointer_ty;
        self.data_model = isa.triple().data_model().ok();
    }

    pub fn subclear(&mut self) {
        for ty in self.subclear_items.drain(..) {
            self.mapping.remove(&ty);
        }
    }
}

pub type Offset = u32;

#[derive(Clone, Copy, Debug)]
pub struct Layout {
    pub size: u32,
    offsets: VSlice<Offset>,
    pub align: NonZeroU8,
    pub repr: Type,
    flags: LayoutFlags,
}

impl Layout {
    pub const EMPTY: Self = Self {
        size: 0,
        align: unsafe { NonZeroU8::new_unchecked(1) },
        offsets: VSlice::empty(),
        repr: types::INVALID,
        flags: LayoutFlags::empty(),
    };

    pub fn rust_layout(&self) -> alloc::Layout {
        alloc::Layout::from_size_align(self.size as usize, self.align.get() as usize).unwrap()
    }

    pub fn from_type(repr: Type) -> Self {
        Self {
            size: repr.bytes(),
            offsets: default(),
            align: (repr.bytes() as u8).try_into().unwrap(),
            repr,
            flags: LayoutFlags::empty(),
        }
    }

    pub fn is_zero_sized(&self) -> bool {
        self.size == 0
    }

    pub fn aligned_to(size: u32, align: u32) -> u32 {
        size + (size as usize as *mut u8).align_offset(align as usize) as u32
    }

    pub fn on_stack(&self) -> bool {
        self.flags.contains(LayoutFlags::ON_STACK)
    }

    pub fn array(&self) -> bool {
        self.flags.contains(LayoutFlags::ARRAY)
    }

    pub fn is_enum(&self) -> bool {
        self.flags.contains(LayoutFlags::ENUM)
    }

    pub fn offsets(self, offsets: &PushMap<u32>) -> impl DoubleEndedIterator<Item = u32> + '_ {
        enum OffsetIter<'a> {
            Array(StepBy<Range<u32>>),
            Struct(Copied<Iter<'a, u32>>),
            Enum(std::array::IntoIter<u32, 2>),
        }

        impl<'a> Iterator for OffsetIter<'a> {
            type Item = u32;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    OffsetIter::Array(iter) => iter.next(),
                    OffsetIter::Struct(iter) => iter.next(),
                    OffsetIter::Enum(iter) => iter.next(),
                }
            }

            fn nth(&mut self, n: usize) -> Option<Self::Item> {
                match self {
                    OffsetIter::Array(iter) => iter.nth(n),
                    OffsetIter::Struct(iter) => iter.nth(n),
                    OffsetIter::Enum(iter) => iter.nth(n),
                }
            }
        }

        impl DoubleEndedIterator for OffsetIter<'_> {
            fn next_back(&mut self) -> Option<Self::Item> {
                match self {
                    OffsetIter::Array(iter) => iter.next_back(),
                    OffsetIter::Struct(iter) => iter.next_back(),
                    OffsetIter::Enum(iter) => iter.next_back(),
                }
            }
        }

        if self.array() {
            OffsetIter::Array((0..self.size).step_by(self.offsets.len()))
        } else if self.is_enum() {
            OffsetIter::Enum([0, self.align.get() as u32].into_iter())
        } else {
            OffsetIter::Struct(offsets[self.offsets].iter().copied())
        }
    }
}

bitflags! {
    LayoutFlags: u8 {
        ON_STACK
        ARRAY
        ENUM
    }
}
