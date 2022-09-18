use std::{fs::*, path::*, process::*, thread::scope};

fn main() {
    println!("Initiating catalyst test!");

    let target = std::env::args().nth(1);

    let dirs_ctor = || {
        read_dir("tests")
            .unwrap()
            .filter_map(|file| file.ok())
            .filter(|file| file.file_type().unwrap().is_dir())
            .map(|file| (file.path(), file.file_name().to_str().unwrap().to_owned()))
            .filter(|(_, name)| !matches!(target, Some(ref target) if !name.contains(target)))
    };

    for (path, name) in dirs_ctor() {
        println!("Compiling test case: {}", name);
        Command::new("cargo")
            .current_dir(path)
            .args(["build"])
            .status()
            .unwrap();
    }

    scope(|h| {
        for (path, name) in dirs_ctor() {
            h.spawn(move || {
                println!("Running test: {}", name);
                Command::new(format!("target/debug/{}.exe", name))
                    .current_dir(path)
                    .status()
                    .unwrap();
                println!("Compiled and Run {}", name);
            });
        }
    });

    for (path, _) in dirs_ctor() {
        let dir_path = path.join("test_out");
        let files = read_dir(&dir_path)
            .unwrap()
            .filter_map(|file| file.ok())
            .filter(|file| file.file_type().unwrap().is_file());

        for file in files {
            let path = file.path();
            let name = path.file_name().unwrap().to_str().unwrap();
            if cmd("git", &dir_path, ["ls-files", "--error-unmatch", name]) == 1 {
                let content = read_to_string(&path).unwrap();
                println!("New file {}:", name);
                println!("{}", content);

                cmd("git", &dir_path, ["add", name]);
                cmd("git", &dir_path, ["commit", "-m", "\"new test result\""]);

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
                cmd("git", &dir_path, ["commit", "-m", "\"test update\""]);
            }
        }
    }
}

fn confirm(message: &str, default_to_yes: bool) -> bool {
    println!(
        "{} [{}]: ",
        message,
        if default_to_yes { "Y/n" } else { "y/N" }
    );

    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let input = input.trim();

    if input.is_empty() {
        return default_to_yes;
    }

    input == "y" || input == "Y"
}

fn cmd<'a>(command: &str, wd: &Path, args: impl IntoIterator<Item = &'a str> + Clone) -> usize {
    let out = Command::new(command)
        .args(args)
        .current_dir(wd)
        .output()
        .expect("failed to execute process");

    out.status.code().unwrap_or(1) as usize
}
