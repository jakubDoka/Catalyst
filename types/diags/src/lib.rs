#![feature(let_else)]
#![feature(default_free_fn)]

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
macro_rules! annotation {
    ($annotation_type:ident$([$id:expr])?$(: $label:tt)?) => {
        (|| Some($crate::Annotation {
            id: $crate::format_message!($($id)?),
            label: $crate::format_message!($($label)?),
            annotation_type: annotation_type!($annotation_type),
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
    (($span:expr $(, $($origin:tt)+)?) $({$($annotations:tt)*})?) => {
        (|| Some($crate::Slice {
            span: $span,
            origin: $crate::slice_origin!($($($origin)+)?),
            annotations: $crate::source_annotations!($($($annotations)*)?),
            fold: true,
        }))()
    };
}

#[macro_export]
macro_rules! source_annotation {
    ($annotation_type:ident$([$span:expr])?$(: $label:tt)?) => {
        (|| Some($crate::SourceAnnotation {
            $(range: $span)?,
            label: $crate::format_message!($($label)?),
            annotation_type: annotation_type!($annotation_type),
            ..Default::default()
        }))()
    };
}

#[macro_export]
macro_rules! source_annotations {
    ($($annotation_type:ident$([$span:expr])?$(: $label:tt)?;)*) => {
        vec![
            $($crate::source_annotation!($annotation_type$([$span])?$(: $label)?)),*
        ]
    };
}

#[macro_export]
macro_rules! sippet {
    {
        $title_type:ident$([$title_id:expr])?$(: $title_label:tt)?;
        $($footer_type:ident$([$footer_id:expr])?$(: $footer_label:tt)?;)*
        $(($span:expr $(, $($origin:tt)+)?) $({$($annotations:tt)*})?)*
        $(opt: $opt:expr)?
    } => {
        Sippet {
            title: $crate::annotation!($title_type$([$title_id])?$(: $title_label)?),
            footer: vec![
                $($crate::annotation!($footer_type$([$footer_id])?$(: $footer_label)?)),*
            ],
            slices: vec![
                $($crate::slice!(($span $(, $($origin)+)?) $({$($annotations)*})?)),*
            ],
            $(opt: $opt,)?
            ..Default::default()
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
            $self.workspace.push($crate::sippet! {
                $($body)*
            })
        }
    };

    {
        $name:ident($($param:ident: $param_ty:ty),* $(,)?) {
            $($body:tt)*
        }
    } => {
        pub fn $name($($param: $param_ty),*) -> $crate::Sippet {
            $crate::sippet! {
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
    Annotation, AnnotationType, FormatOptions, Margin, Sippet, Slice, SourceAnnotation, Str,
    Workspace,
};

#[allow(unused)]
#[cfg(test)]
mod tests {
    use lexing_t::Span;

    use super::*;

    #[test]
    fn test() {
        drop(sippet! {
            err["id"]: "hello";
            err: "world";
            (Span::new(0..0)) {
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
                (span) {
                    err[span]: "world";
                }
            }
        }
    }
}
