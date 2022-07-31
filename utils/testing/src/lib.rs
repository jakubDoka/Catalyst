#![feature(scoped_threads)]

pub use crate::testing::{test_case, Folder};

#[macro_export]
macro_rules! gen_test {
    ($($($type:ident)? $name:literal {
        $($structure:tt)*
    })*) => {
        std::thread::scope(|h| {
            $(
                test_case($name, None, |name| {
                    gen_test!(__inner__ name $($type)? $($structure)*);
                    TestState::run(name)
                });
            )*
        });
    };

    (__inner__ $name:ident simple $($structure:tt)*) => {
        quick_file_system!(
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
            let mut __dir = $crate::Folder::new($root);
            quick_file_system!(__recur__ (__dir) $($tokens)*);
            __dir.create();
            __dir
        };
    };

    (__recur__ ($parent:expr) $($key:ident $name:literal {$($content:tt)*})*) => {
        $(
            quick_file_system!(__item__ ($parent) $key $name {$($content)*});
        )*
    };

    (__item__ ($parent:expr) file $name:literal {$($content:tt)*}) => {
        $parent.files.push(($name.to_string(), stringify!($($content)*).to_string()));
    };

    (__item__ ($parent:expr) dir $name:literal {$($content:tt)*}) => {
        let mut __dir = $crate::Folder::new($name);
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

mod testing {
    use ansi_coloring::*;
    use diags::*;
    use packaging_t::*;
    use std::{path::*, thread::Scope};

    pub fn test_case<'a: 'b, 'b, 'c>(
        name: &'static str,
        scope: Option<&'a Scope<'b, 'c>>,
        test_code: fn(&str) -> (Workspace, Packages),
    ) {
        let runner = move || {
            let (ws, packages) = test_code(name);

            let mut out = String::new();
            ws.display(&packages, &mut out, &Style::NONE).unwrap();

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

        pub fn create(&mut self) {
            let path = PathBuf::from(&self.name);
            self.create_recur(&path);
            self.global_path = Some(path);
        }

        fn create_recur(&self, path: &Path) {
            let self_path = path;
            if !path.exists() {
                std::fs::create_dir(&self_path).unwrap();
            }
            for (name, content) in &self.files {
                std::fs::write(
                    self_path.join(name),
                    content.replace('\n', " ").replace("::", "`"),
                )
                .unwrap();
            }

            for folder in &self.folders {
                folder.create_recur(&path.join(&folder.name));
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
