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
            Ty::Array(_) => todo!(),
        };

        if creator.typec.contains_params(ty) {
            self.subclear_items.push(ty);
        }

        self.mapping.insert(ty, res);

        res
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
        let offsets = [0, align as u32];
        let size = Layout::aligned_to(tag_size + base_size, align as u32);
        let (repr, on_stack) = self.repr_for_size(size);

        Layout {
            size,
            offsets: self.offsets.extend(offsets),
            align: align.try_into().unwrap(),
            repr,
            on_stack,
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
            on_stack,
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
    pub offsets: VSlice<Offset>,
    pub align: NonZeroU8,
    pub repr: Type,
    pub on_stack: bool,
}

impl Layout {
    pub const EMPTY: Self = Self {
        size: 0,
        align: unsafe { NonZeroU8::new_unchecked(1) },
        offsets: VSlice::empty(),
        repr: types::INVALID,
        on_stack: false,
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
            on_stack: false,
        }
    }

    pub fn is_zero_sized(&self) -> bool {
        self.size == 0
    }

    pub fn aligned_to(size: u32, align: u32) -> u32 {
        size + (size as usize as *mut u8).align_offset(align as usize) as u32
    }
}
