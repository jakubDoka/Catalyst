#![feature(if_let_guard, default_free_fn, iter_intersperse)]
mod context;
mod creation;

pub use {
    context::TypeCreator,
    creation::display::{
        display, display_array, display_bin_op, display_func_name, display_instance, display_list,
        display_spec_sum, type_diff, TypeDisplay,
    },
};
