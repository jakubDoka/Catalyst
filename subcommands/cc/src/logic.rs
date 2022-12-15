use std::{fs, path::Path, process::Command};

use crate::*;
use cli::*;
use middleware::annotate_snippets::display_list::FormatOptions;

pub struct CcRuntime {
    middleware: Middleware,
    cli_input: CliInput,
    args: Option<MiddlewareArgs>,
}

impl CcRuntime {
    pub fn new(cli_input: CliInput) -> Self {
        Self {
            middleware: Middleware::new(),
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
        fn host_isa() -> Option<Isa> {
            Isa::host(false)
                .inspect_err(|e| eprintln!("Problem with host architecture: {e}"))
                .ok()
        }

        let args = match self.args {
            Some(ref mut args) => args,
            None => {
                let isa = match self.cli_input.value("target") {
                    Some(_triple) => todo!(),
                    None => host_isa()?,
                };

                self.args.insert(MiddlewareArgs {
                    path: self
                        .cli_input
                        .args()
                        .get(1)
                        .cloned()
                        .unwrap_or(".".into())
                        .into(),
                    jit_isa: host_isa()?,
                    isa,
                    incremental_path: self.cli_input.value("incremental-path").map(|s| s.into()),
                    max_cores: self
                        .cli_input
                        .value("max-cores")
                        .and_then(|s| s.parse().ok()),
                    dump_ir: self.cli_input.enabled("dump-ir"),
                    check: self.cli_input.enabled("check"),
                })
            }
        };

        let Some(output) = self.middleware.update(args) else {
            let Some(view) = self.middleware.diagnostic_view() else {
                eprintln!("Compilation failed no diagnostic view.");
                return None;
            };

            if view.resources.no_changes() {
                println!("No changes detected.");
                return None;
            }

            if !view.workspace.has_errors() {
                println!("No errors found.");
                return None;
            }

            let mut display = SnippetDisplayImpl {
                opts: FormatOptions {
                    color: true,
                    ..Default::default()
                },
                tab_width: 4,
            };

            let diagnostics = view.workspace.display(view.resources, &mut display);

            println!("{diagnostics}");
            println!("Compilation failed.");

            return None;
        };

        let dest = self.cli_input.value("output").unwrap_or("a");

        let exe_path = format!("{dest}.exe");
        let obj_path = format!("{dest}.obj");

        fs::write(&obj_path, output.binary).unwrap();

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
