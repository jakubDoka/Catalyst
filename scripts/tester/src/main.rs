use std::{
    fs::{self, *},
    io::Write,
    path::*,
    process::*,
    thread::scope,
};

fn main() {
    println!("Initiating catalyst test!");

    let target = std::env::args().nth(1);

    let dirs_ctor = || {
        read_dir("tests")
            .unwrap()
            .filter_map(|file| file.ok())
            .filter(|file| file.file_type().unwrap().is_dir())
            .map(|file| (file.path(), file.file_name().to_str().unwrap().to_owned()))
            .filter(|(.., name)| !matches!(target, Some(ref target) if !name.contains(target)))
    };

    for (path, name) in dirs_ctor() {
        println!("Compiling test case: {name}");
        Command::new("cargo")
            .current_dir(path)
            .args(["build", "--release"])
            .status()
            .unwrap();
    }

    scope(|h| {
        for (path, name) in dirs_ctor() {
            h.spawn(move || {
                println!("Running test: {name}");
                Command::new(
                    Path::new(&format!("target/debug/{name}"))
                        .canonicalize()
                        .unwrap(),
                )
                .current_dir(path)
                .args(std::env::args().nth(2))
                .status()
                .unwrap();
                println!("Compiled and Run {name}");
            });
        }
    });

    let mut any_changes = false;
    for (path, _) in dirs_ctor() {
        let dir_path = path.join("test_out");
        if !dir_path.exists() {
            fs::create_dir(&dir_path).unwrap();
        }
        let files = read_dir(&dir_path)
            .unwrap()
            .filter_map(|file| file.ok())
            .filter(|file| file.file_type().unwrap().is_file());

        for file in files {
            let path = file.path();
            let name = path.file_name().unwrap().to_str().unwrap();
            if cmd("git", &dir_path, ["ls-files", "--error-unmatch", name]) == 1 {
                let content = read_to_string(&path).unwrap();
                println!("New file {name}:");
                println!("{content}");

                cmd("git", &dir_path, ["add", name]);
                any_changes = true;
                continue;
            }

            let changed = !Command::new("git")
                .args(["diff", "--exit-code"])
                .arg(&path)
                .status()
                .unwrap()
                .success();

            if changed && confirm("Do you want to commit?", false) {
                cmd("git", &dir_path, ["add", name]);
                any_changes = true;
            }
        }
    }

    if any_changes {
        let input = prompt("Enter commit message");
        cmd("git", Path::new("."), ["commit", "-m", &input]);
    }
}

fn prompt(msg: &str) -> String {
    print!("{msg}:");
    std::io::stdout().flush().unwrap();
    read_input()
}

fn confirm(message: &str, default_to_yes: bool) -> bool {
    println!(
        "{} [{}]: ",
        message,
        if default_to_yes { "Y/n" } else { "y/N" }
    );

    let input = read_input();

    if input.is_empty() {
        return default_to_yes;
    }

    input == "y" || input == "Y"
}

fn read_input() -> String {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input.trim().to_owned()
}

fn cmd<'a>(command: &str, wd: &Path, args: impl IntoIterator<Item = &'a str> + Clone) -> usize {
    let out = Command::new(command)
        .args(args)
        .current_dir(wd)
        .output()
        .expect("failed to execute process");

    out.status.code().unwrap_or(1) as usize
}
