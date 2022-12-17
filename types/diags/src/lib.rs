#![feature(default_free_fn)]
#![feature(stmt_expr_attributes)]
#![allow(clippy::redundant_closure_call)]
#![feature(never_type)]

#[macro_export]
macro_rules! annotation_type {
    (err) => {
        $crate::CtlAnnotationType::Error
    };
    (warn) => {
        $crate::CtlAnnotationType::Warning
    };
    (info) => {
        $crate::CtlAnnotationType::Info
    };
    (note) => {
        $crate::CtlAnnotationType::Note
    };
    (help) => {
        $crate::CtlAnnotationType::Help
    };
}

#[macro_export]
macro_rules! ctl_error_message {
    ($string:literal) => {
        format!($string)
    };
    (($($tokens:tt)*)) => {
        format!($($tokens)*)
    };
    ($string:expr) => {
        $string.into()
    };
}

#[macro_export]
macro_rules! ctl_error_id {
    () => {
        None
    };
    ($id:ident) => {
        Some(stringify!($id).into())
    };
}

#[macro_export]
macro_rules! annotation {
    ($annotation_type:ident $($id:ident)? => $($label:tt)+) => {
        $crate::CtlAnnotation {
            id: $crate::ctl_error_id!($($id)?),
            label: $crate::ctl_error_message!($($label)+),
            annotation_type: $crate::annotation_type!($annotation_type),
        }
    };
}

#[macro_export]
macro_rules! source_annotation {
    ($annotation_type:ident $source:expr, $span:expr, $($label:tt)+) => {
        $crate::CtlSourceAnnotation {
            origin: $source,
            span: $span,
            label: $crate::ctl_error_message!($($label)+),
            annotation_type: $crate::annotation_type!($annotation_type),
        }
    };

    ($annotation_type:ident $loc:expr, $($label:tt)+) => {
        $crate::source_annotation!($annotation_type $loc.origin, $loc.span, $($label)+)
    };

    ($annotation_type:ident $loc:expr) => {
        $crate::source_annotation!($annotation_type $loc, "here")
    };
}

#[macro_export]
macro_rules! ctl_error_fatality {
    () => {
        false
    };
    (fatal) => {
        true
    };
}

#[macro_export]
macro_rules! ctl_errors {
    (
        $(
            #[$($title:tt)*]
            $(#[$($footer:tt)*])*
            $($fatality:ident)? struct $name:ident {
                $(#[$($source:tt)*])*
                $($field:ident $($ref:ident)?: $field_type:ty),* $(,)?
            }
        )*
    ) => {
        $(
            pub struct $name {
                $(pub $field: $field_type),*
            }

            impl $crate::CtlError for $name {
                fn is_fatal(&self) -> bool {
                    $crate::ctl_error_fatality!($($fatality)?)
                }

                fn fill_snippet<'a>(&self, snippet: &mut $crate::CtlSnippet) {
                    let &$name { $( $($ref)? $field),* } = self;

                    snippet.title = $crate::annotation!($($title)*);
                    $(
                        snippet.footer.push($crate::annotation!($($footer)*));
                    )*

                    $(
                        snippet.source_annotations.push(
                            $crate::source_annotation!($($source)*)
                        );
                    )*
                }
            }
        )*
    };
}

mod items;

pub use items::{
    CtlAnnotation, CtlAnnotationType, CtlError, CtlSnippet, CtlSourceAnnotation, ErrorCount,
    SnippetDisplay, SourceLoc, Workspace,
};

#[allow(unused)]
#[cfg(test)]
mod tests {
    use lexing_t::Span;
    use storage::*;

    use super::*;

    ctl_errors! {
        #[err afg32 => "hello I need {amount} of {food}"]
        #[err => "something happened"]
        fatal struct BeggingForFood {
            #[info location, "here"]
            location: SourceLoc,
            amount: i32,
            food ref: String,
        }
    }
}
