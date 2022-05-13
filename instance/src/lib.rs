#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(bool_to_option)]
#![feature(if_let_guard)]

pub mod error;
pub mod func;
pub mod repr;

pub use func::MirBuilder;
pub use repr::ReprBuilder;

#[macro_export]
macro_rules! repr_builder {
    ($self:expr) => {
        ReprBuilder::new(
            $self.types,
            $self.sources,
            $self.sfields,
            $self.instances,
            $self.ty_lists,
            $self.repr_fields,
            $self.reprs,
            $self.ptr_ty,
        )
    };
}
