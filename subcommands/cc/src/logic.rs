use std::{fs, path::Path, process::Command};

use crate::*;
use cli::*;

pub struct CcRuntime<'m> {
    middleware: &'m mut Middleware,
    cli_input: CliInput,
    args: Option<MiddlewareArgs>,
}

impl<'m> CcRuntime<'m> {
    pub fn new(cli_input: CliInput, middleware: &'m mut Middleware) -> Self {
        Self {
            middleware,
            cli_input,
            args: None,
        }
    }

    pub fn run(mut self) {
        if self.cli_input.enabled("one-shot") {
            self.compile();
            return;
        }

        let dialogue = CliDialogue {
            prompt: "cc >>".to_string(),
            help: "todo".to_string(),
            exit_word: "exit".to_string(),
        };

        for input in dialogue {
            match input.args() {
                [] => self.compile(),
                [arg] if arg.starts_with('c') => self.compile(),
                _ => {
                    eprintln!("invalid input");
                    None
                }
            };
        }
    }

    pub fn compile(&mut self) -> Option<()> {
        let args = match self.args {
            Some(ref mut args) => args,
            None => self.args.insert(
                MiddlewareArgs::from_cli_input(&self.cli_input)
                    .map_err(|e| eprintln!("Problem with architecture: {e}"))
                    .ok()?,
            ),
        };

        let (output, view) = self.middleware.update(args, &mut OsResources);
        let (binary, _ir) = match view.dump_diagnostics(true, output) {
            Ok(output) => output,
            Err(err) => {
                eprintln!("{err}");
                return None;
            }
        };

        let dest = self.cli_input.value("output").unwrap_or("a");

        let exe_path = format!("{dest}.exe");
        let obj_path = format!("{dest}.obj");

        fs::write(&obj_path, binary).unwrap();

        if self.cli_input.enabled("obj") {
            return None;
        }

        let host = args.isa.triple().to_string();

        let compiler = cc::Build::new()
            .opt_level(0)
            .target(&host)
            .host(&host)
            .cargo_metadata(false)
            .get_compiler();

        let args = if compiler.is_like_msvc() {
            vec![
                "ucrt.lib".into(),
                "vcruntime.lib".into(),
                format!("-link /ENTRY:{ENTRY_POINT_NAME} /SUBSYSTEM:CONSOLE"),
            ]
        } else if compiler.is_like_clang() {
            todo!()
        } else if compiler.is_like_gnu() {
            todo!()
        } else {
            unimplemented!("unknown compiler");
        };

        compiler
            .to_command()
            .arg(&obj_path)
            .args(args)
            .status()
            .unwrap();

        let path = Path::new(&exe_path).canonicalize().unwrap();

        fs::remove_file(obj_path).unwrap();

        if self.cli_input.enabled("run") {
            println!("Running: {}", path.display());
            let output = Command::new(path)
                .current_dir(std::env::current_dir().unwrap())
                .status();
            match output {
                Ok(status) => println!("Exited with status: {status}"),
                Err(err) => println!("Failed to run the executable: {err}"),
            }
        } else {
            println!("Compiled to: {}", path.display());
        }

        None
    }
}
