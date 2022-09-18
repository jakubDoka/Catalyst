#![feature(let_else)]
#![feature(default_free_fn)]
#![feature(stmt_expr_attributes)]
#![allow(clippy::redundant_closure_call)]

#[macro_export]
macro_rules! annotation_type {
    (err) => {
        $crate::AnnotationType::Error
    };
    (warn) => {
        $crate::AnnotationType::Warning
    };
    (info) => {
        $crate::AnnotationType::Info
    };
    (note) => {
        $crate::AnnotationType::Note
    };
    (help) => {
        $crate::AnnotationType::Help
    };
}

#[macro_export]
macro_rules! format_message {
    () => {
        None
    };
    (($($tokens:tt)*)) => {
        Some(format!($($tokens)*).into())
    };
    ($string:expr) => {
        Some($string.into())
    };
}

#[macro_export]
macro_rules! format_required_message {
    (($($tokens:tt)*)) => {
        format!($($tokens)*).into()
    };
    ($string:expr) => {
        $string.into()
    };
}

#[macro_export]
macro_rules! annotation {
    ($annotation_type:ident$([$id:expr])?$(: $label:tt)?) => {
        (|| Some($crate::Annotation {
            id: $crate::format_message!($($id)?),
            label: $crate::format_message!($($label)?),
            annotation_type: $crate::annotation_type!($annotation_type),
        }))()
    };
}

#[macro_export]
macro_rules! slice_origin {
    () => {
        None.into()
    };
    ($origin:expr) => {
        $origin.into()
    };
}

#[macro_export]
macro_rules! slice {
    (($span:expr, $origin:expr) $({$($annotations:tt)*})?) => {
        (|| Some($crate::Slice {
            span: $span,
            origin: $origin,
            annotations: $crate::source_annotations!($($($annotations)*)?),
            fold: true,
        }))()
    };
}

#[macro_export]
macro_rules! source_annotation {
    ($annotation_type:ident$([$span:expr])?: $label:tt) => {
        #[allow(clippy::needless_update)]
        {
            (|| Some($crate::SourceAnnotation {
                $(range: $span)?,
                label: $crate::format_required_message!($label),
                annotation_type: $crate::annotation_type!($annotation_type),
                ..Default::default()
            }))()
        }
    };
}

#[macro_export]
macro_rules! source_annotations {
    ($($annotation_type:ident$([$span:expr])?: $label:tt;)*) => {
        vec![
            $($crate::source_annotation!($annotation_type$([$span])?: $label)),*
        ]
    };
}

#[macro_export]
macro_rules! snippet {
    {
        $title_type:ident$([$title_id:expr])?$(: $title_label:tt)?;
        $($footer_type:ident$([$footer_id:expr])?$(: $footer_label:tt)?;)*
        $(($span:expr, $origin:expr) $({$($annotations:tt)*})?)*
        $(opt: $opt:expr)?
    } => {
        $crate::Snippet {
            title: $crate::annotation!($title_type$([$title_id])?$(: $title_label)?),
            footer: vec![

                $($crate::annotation!($footer_type$([$footer_id])?$(: $footer_label)?),)*
            ],
            slices: vec![
                $($crate::slice!(($span, $origin) $({$($annotations)*})?)),*
            ],
            origin: format!("Error originated from '{}:{}'", file!(), line!()),
        }
    };
}

#[macro_export]
macro_rules! gen_error_fn {
    {
        push $name:ident($self:ident, $($param:ident: $param_ty:ty),* $(,)?) {
            $($body:tt)*
        }
    } => {
        pub fn $name(&mut $self, $($param: $param_ty),*) {
            $self.workspace.push($crate::snippet! {
                $($body)*
            })
        }
    };

    {
        print $name:ident($self:ident, $($param:ident: $param_ty:ty),* $(,)?) {
            $($body:tt)*
        }
    } => {
        pub fn $name(&mut $self, $($param: $param_ty),*) {
            $self.workspace.push_or_display(&$self.packages, $crate::snippet! {
                $($body)*
            })
        }
    };

    {
        $name:ident($($param:ident: $param_ty:ty),* $(,)?) {
            $($body:tt)*
        }
    } => {
        pub fn $name($($param: $param_ty),*) -> $crate::Snippet {
            $crate::snippet! {
                $($body)*
            }
        }
    };
}

#[macro_export]
macro_rules! gen_error_fns {
    (
        $(
            $prefix:ident $($name:ident)? ($($params:tt)*) {
                $($body:tt)*
            }
        )*
    ) => {
        $(
            $crate::gen_error_fn!(
                $prefix $($name)? ($($params)*) {
                    $($body)*
                }
            );
        )*
    };
}

mod items;

pub use items::{
    Annotation, AnnotationType, ErrorCount, Slice, Snippet, SnippetDisplay, SourceAnnotation, Str,
    Workspace,
};

#[allow(unused)]
#[cfg(test)]
mod tests {
    use lexing_t::Span;
    use storage::*;

    use super::*;

    #[test]
    fn test() {
        drop(snippet! {
            err["id"]: "hello";
            err: "world";
            (Span::new(0..0), Interner::EMPTY) {
                err[Span::new(0..0)]: "world";
            }
        });
    }

    struct A {
        workspace: Workspace,
    }

    impl A {
        gen_error_fns! {
            push test_err(self, span: Span, a: i32, b: i32) {
                err["goo"]: ("hello {} {}", a, b);
                (span, Interner::EMPTY) {
                    err[span]: "world";
                }
            }
        }
    }
}
