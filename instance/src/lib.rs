#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(bool_to_option)]
#![feature(if_let_guard)]

pub mod error;
pub mod func;
pub mod repr;

pub use func::MirBuilder;
pub use repr::{LayoutBuilder, ReprInstancing};

#[macro_export]
macro_rules! layout_builder {
    ($self:expr) => {
        layout_builder!($self, $self.ptr_ty)
    };

    ($self:expr, $ptr_ty:expr) => {
        LayoutBuilder::new(
            &$self.types,
            &$self.sources,
            &$self.ty_comps,
            &$self.instances,
            &$self.ty_lists,
            &mut $self.repr_fields,
            &mut $self.reprs,
        )
    };
}
