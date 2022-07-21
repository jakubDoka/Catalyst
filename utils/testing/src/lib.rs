pub use crate::testing::{test_case, Folder};

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
    use std::path::*;

    pub fn test_case(name: &str, test_code: impl Fn() -> (Workspace, Packages)) {
        let (ws, packages) = test_code();

        let mut out = String::new();
        ws.display(&packages, &mut out, &Style::NONE).unwrap();

        let path = format!("{}/{}.txt", "test_out", name);
        if !Path::new("test_out").exists() {
            std::fs::create_dir("test_out").unwrap();
        }
        std::fs::write(path, out).unwrap();
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
                std::fs::write(self_path.join(name), content.replace('\n', " ")).unwrap();
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
