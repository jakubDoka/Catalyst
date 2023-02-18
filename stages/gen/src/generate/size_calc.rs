use super::*;

impl Generator<'_> {
    pub fn ty_repr(&mut self, ty: Ty) -> Type {
        self.ty_layout(ty).repr
    }

    pub fn ty_layout(&mut self, ty: Ty) -> Layout {
        self.ty_layout_low(ty, &[])
    }

    fn ty_layout_low(&mut self, ty: Ty, params: &[Ty]) -> Layout {
        self.layouts.ty_layout(ty, params, type_creator!(self))
    }

    pub fn is_representable(&mut self, value: VRef<ValueMir>, builder: &mut GenBuilder) -> bool {
        self.ty_layout(builder.value_ty(value)).size != 0
    }
}
