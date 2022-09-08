#![feature(closure_lifetime_binder)]

#[macro_export]
macro_rules! gen_test {
    (
        $test_struct:ty,
        $parallel:literal,
        $($($type:ident)? $name:literal {
            $($structure:tt)*
        })*
    ) => {
        std::thread::scope(|h| {
            fn testable<T: $crate::items::Testable>() {}
            testable::<$test_struct>();
            $(
                let value = $parallel.then_some(h);
                $crate::items::test_case($name, value, |name| {
                    gen_test!(__inner__ name $($type)? $($structure)*);

                    <$test_struct>::run(name)
                });
            )*
        });
    };

    (__inner__ $name:ident simple $($structure:tt)*) => {
        $crate::quick_file_system!(
            ($name)
            file "root.ctl" {
                $($structure)*
            }
            file "package.ctlm" {}
        )
    };

    (__inner__ $name:ident $($structure:tt)*) => {
        quick_file_system!(
            ($name)
            $($structure)*
        )
    };
}

#[macro_export]
macro_rules! quick_file_system {
    (($root:expr) $($tokens:tt)*) => {
        let __dir = {
            let mut __dir = $crate::items::Folder::new($root);
            $crate::quick_file_system!(__recur__ (__dir) $($tokens)*);
            __dir.create();
            __dir
        };
    };

    (__recur__ ($parent:expr) $($key:ident $name:literal {$($content:tt)*})*) => {
        $(
            $crate::quick_file_system!(__item__ ($parent) $key $name {$($content)*});
        )*
    };

    (__item__ ($parent:expr) file $name:literal {$($content:tt)*}) => {
        $parent.files.push(($name.to_string(), stringify!($($content)*).to_string()));
    };

    (__item__ ($parent:expr) dir $name:literal {$($content:tt)*}) => {
        let mut __dir = $crate::items::Folder::new($name);
        quick_file_system!(__recur__ (__dir) $($content)*);
        $parent.folders.push(__dir);
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        quick_file_system!(
            ("foo")
            file "root.ctl" {
                fn main() {
                    "Hello, world!".log()
                }
            }
            file "package.ctlm" {
                root: "root.ctl";
            }
        );
    }
}

pub use items::Testable;

pub mod items {
    use ansi_coloring::*;
    use diags::*;
    use fmt::Fmt;
    use packaging_t::*;
    use std::{path::*, thread::Scope};
    use storage::{Ident, Interner};

    pub trait Testable {
        fn run(name: &str) -> (Workspace, Packages);
    }

    pub fn test_case<'a: 'b, 'b, 'c>(
        name: &'static str,
        scope: Option<&'a Scope<'b, 'c>>,
        test_code: fn(&str) -> (Workspace, Packages),
    ) {
        let runner = move || {
            let (ws, packages) = test_code(name);

            let mut out = String::new();
            //ws.display(&packages, &mut out, &Style::NONE).unwrap();

            let path = format!("{}/{}.txt", "test_out", name);
            if !Path::new("test_out").exists() {
                std::fs::create_dir("test_out").unwrap();
            }
            std::fs::write(path, out).unwrap();
        };

        if let Some(scope) = scope {
            scope.spawn(runner);
        } else {
            runner();
        }
    }

    pub struct Folder {
        pub name: String,
        pub files: Vec<(String, String)>,
        pub folders: Vec<Folder>,
        global_path: Option<PathBuf>,
    }

    impl Folder {
        pub fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
                files: Vec::new(),
                folders: Vec::new(),
                global_path: None,
            }
        }

        pub fn create(&mut self) -> Workspace {
            let mut fmt = Fmt::new();
            let path = PathBuf::from(&self.name);
            let mut interner = Interner::new();
            self.create_recur(&path, &mut fmt, &mut interner);
            self.global_path = Some(path);
            fmt.into_workspace()
        }

        fn create_recur(&self, path: &Path, fmt: &mut Fmt, interner: &mut Interner) {
            let self_path = path;
            if !path.exists() {
                std::fs::create_dir(&self_path).unwrap();
            }
            for (name, content) in &self.files {
                let formatter = if name.ends_with(".ctl") {
                    Fmt::source
                //} else if name.ends_with(".ctlm") {
                // TODO
                } else {
                    for<'a> |_: &'a mut Fmt, s: String, _: Ident| -> (Option<&'a str>, String) {
                        (None, s)
                    }
                };

                let path = self_path.join(name);
                let path_ident = interner.intern_str(path.to_str().unwrap());

                let c = content.replace('\n', " ").replace("::", "`");
                let (res, c) = formatter(fmt, c, path_ident);

                let res = res.unwrap_or(&c);

                std::fs::write(path, res).unwrap();
            }

            for folder in &self.folders {
                folder.create_recur(&path.join(&folder.name), fmt, interner);
            }
        }
    }

    impl Drop for Folder {
        fn drop(&mut self) {
            if let Some(path) = &self.global_path {
                std::fs::remove_dir_all(&path).unwrap();
            }
        }
    }
}
