#![feature(
    associated_type_defaults,
    const_type_id,
    default_free_fn,
    anonymous_lifetime_in_impl_trait,
    let_chains,
    never_type,
    iter_intersperse,
    try_blocks,
    result_option_inspect,
    if_let_guard,
    slice_group_by,
    iter_collect_into
)]

macro_rules! lookup {
    ($what:ident $self:expr, $id:expr, $span:expr) => {
        match $self.lookup($id, $span, stringify!($what))? {
            ScopeItem::$what(func) => func,
            item => $self.invalid_symbol_type(item, $span, stringify!($what))?,
        }
    };
}

mod context;
mod func_builder;
mod tir_display;
mod ty_parser;

pub use {
    context::{TypecCtx, TypecExternalCtx, TypecMeta},
    func_builder::TirBuilder,
    ty_parser::TypecParser,
};
