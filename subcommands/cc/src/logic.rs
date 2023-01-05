use std::{error::Error, fmt::Write, fs, path::Path, process::Command};

use crate::*;
use cli::*;

#[derive(Default)]
pub struct CcCtx {
    input: Option<CliInput>,
    cached_exe: Option<Vec<u8>>,
}

pub struct CcRuntime<'m> {
    middleware: &'m mut Middleware,
    ctx: &'m mut CcCtx,
}

impl<'m> CcRuntime<'m> {
    command_info! {
        HELP
        [
            "ctl c [...]\n",
            "Allows invoking Catalyst compiler to produce platform specific executable files.\n",
        ]: MiddlewareArgs::HELP;
        flags {
            "obj" => "output just object file"
            "run" => "run right after compilation"
        }
        values {
            "output"("string = 'a'") => "output file name"
        }
    }

    pub fn new(middleware: &'m mut Middleware, ctx: &'m mut CcCtx) -> Self {
        Self { middleware, ctx }
    }

    pub fn run(mut self, input: CliInput) -> Result<(), Box<dyn Error>> {
        let res = self.compile(&input)?;
        self.ctx.input = Some(input);
        Ok(res)
    }

    fn compile(&mut self, input: &CliInput) -> Result<(), Box<dyn Error>> {
        fn exe_path(input: &CliInput) -> String {
            let dest = input.value("output").unwrap_or("a");
            if cfg!(unix) {
                dest.into()
            } else {
                format!("{dest}.exe")
            }
        }

        let mid_args = MiddlewareArgs::from_cli_input(&input, Self::HELP)?;

        let (output, view) = self.middleware.update(&mid_args, &mut OsResources);
        let unchanged = matches!(output, MiddlewareOutput::Unchanged);
        let failure = matches!(output, MiddlewareOutput::Failed);
        let exe_path = exe_path(&input);
        let err = match view.dump_diagnostics(true, output) {
            Ok((binary, _ir)) => {
                let obj_path = format!("ctl_o.obj");

                fs::write(&obj_path, binary)?;

                if input.enabled("obj") {
                    return Ok(());
                }

                let host = mid_args.isa.triple().to_string();

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
                    vec![
                        format!("-o{}", exe_path),
                        "-nostartfiles".into(),
                        format!("-e{}", middleware::ENTRY_POINT_NAME),
                    ]
                } else {
                    unimplemented!("unknown compiler");
                };

                compiler
                    .to_command()
                    .arg(&obj_path)
                    .args(args)
                    .status()
                    .unwrap();

                fs::remove_file(obj_path).unwrap();

                Ok(())
            }
            Err(err) if failure => return Err(err.into()),
            Err(err) => Err(err),
        };

        if let Some(ref current_input) = self.ctx.input
            && let Some(ref cached_exe) = self.ctx.cached_exe
            && unchanged
            && (input == current_input || input.args().first().map_or(false, |s| s.starts_with('r')))
        {
            fs::write(&exe_path, cached_exe)?;
        } else if let Ok(bytes) = fs::read(&exe_path) {
            self.ctx.cached_exe = Some(bytes);
        }

        let path = Path::new(&exe_path).canonicalize()?;

        if input.enabled("run") {
            writeln!(mid_args.display(), "Running: {}", path.display())?;
            let output = Command::new(path)
                .current_dir(std::env::current_dir().unwrap())
                .status();
            match output {
                Ok(status) => writeln!(mid_args.display(), "Exited with status: {status}"),
                Err(err) => writeln!(mid_args.display(), "Failed to run the executable: {err}"),
            }?;
        } else {
            writeln!(mid_args.display(), "Compiled to: {}", path.display())?;
        }

        Ok(err?)
    }
}
