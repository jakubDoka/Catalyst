use std::{
    fmt::Write,
    fs, io,
    path::Path,
    process::{Command, ExitStatus, Stdio},
};

use crate::*;
use cli::*;

compose_error! {
    CcError {
        #["failed to write oject file: {inner}", ]
        ObjectWriteFail(inner: io::Error),
        #["incremental object file was unexpectedly deleted"]
        ObjectDeleted,
        #["the system c compiler is neither like gcc, clang, or msvc"]
        UnrecognisedCcFamily,
        #["failed to invoke system c compiler: {inner}"]
        CcInvokeFailure(inner: io::Error),
        #["linker exited with '{}', stderr:\n{}", inner.0, inner.1]
        LinkingFailed(inner: (ExitStatus, String)),
        #["{inner}"]
        MiddlewareArgs(inner: MiddlewareArgsError),
        #["interupritn linking due to '-obj'"]
        JustObject,
        #["failed to get absolute path of executable: {inner}"]
        CanonicalizeExe(inner: io::Error),
        #["{inner}"]
        Diagnostics(inner: String),
    }
}

type Result<T = ()> = std::result::Result<T, CcError>;

#[derive(Default)]
pub struct CcCtx {
    input: Option<CliInput>,
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
            "link-with"("string = ''") => "list of object files to link with executable"
        }
    }

    pub fn new(middleware: &'m mut Middleware, ctx: &'m mut CcCtx) -> Self {
        Self { middleware, ctx }
    }

    pub fn run(mut self, input: CliInput) -> Result {
        let res = self.compile(&input)?;
        self.ctx.input = Some(input);
        Ok(res)
    }

    fn compile(&mut self, input: &CliInput) -> Result {
        let mid_args =
            MiddlewareArgs::from_cli_input(&input, Self::HELP).map_err(CcError::MiddlewareArgs)?;

        let (output, view) = self.middleware.update(&mid_args, &mut OsResources);
        let binary = view.dump_diagnostics(true, &output).map(|(bin, ir)| {
            if let Some(ir) = ir {
                let _ = writeln!(mid_args.display(), "{ir}");
            }
            bin
        });

        if mid_args.check && let MiddlewareOutput::Checked = output {
            return Ok(());
        }

        if let MiddlewareOutput::Failed = output {
            binary.map_err(CcError::Diagnostics)?;
            unreachable!();
        }

        let exe_path = Self::link(&mid_args, input, binary.as_ref().ok().copied())?;

        let path = Path::new(&exe_path)
            .canonicalize()
            .map_err(CcError::CanonicalizeExe)?;

        if input.enabled("run") {
            let _ = writeln!(mid_args.display(), "Running: {}", path.display());
            let timer = QuickTimer::new("up time");
            let output = Command::new(path).status();
            timer.drop();
            let _ = match output {
                Ok(status) => writeln!(mid_args.display(), "Exited with status: {status}"),
                Err(err) => writeln!(mid_args.display(), "Failed to run the executable: {err}"),
            };
        } else {
            let _ = write!(mid_args.display(), "Compiled to: {}", path.display());
        }

        binary.map_err(CcError::Diagnostics)?;

        Ok(())
    }

    fn link(mid_args: &MiddlewareArgs, input: &CliInput, binary: Option<&[u8]>) -> Result<String> {
        fn exe_path(input: &CliInput) -> String {
            let dest = input.value("output").unwrap_or("a");
            if cfg!(unix) {
                dest.into()
            } else {
                format!("{dest}.exe")
            }
        }

        let _t = QuickTimer::new("linking");

        const OBJ_NAME: &str = "ctl.o";

        let obj_path = mid_args.incremental_root().join(OBJ_NAME);

        if let Some(binary) = binary {
            if let Some(parent) = obj_path.parent() {
                fs::create_dir_all(parent).map_err(CcError::ObjectWriteFail)?;
            }
            fs::write(&obj_path, binary).map_err(CcError::ObjectWriteFail)?;
        } else if !obj_path.exists() {
            return Err(CcError::ObjectDeleted);
        }

        if input.enabled("obj") {
            return Err(CcError::JustObject);
        }

        let triple = &mid_args.isa.triple();
        let target = triple.to_string();

        enum LinkerFamily {
            Gnu,
            Clang,
            Mswc,
        }
        use LinkerFamily::*;

        let compiler = cc::Build::new()
            .opt_level(0)
            .target(&target)
            .host(&target)
            .cargo_metadata(false)
            .get_compiler();

        let choices = [
            (compiler.is_like_gnu(), Gnu),
            (compiler.is_like_clang(), Clang),
            (compiler.is_like_msvc(), Mswc),
        ];

        let Some((.., family)) = choices.into_iter().find(|&(y, ..)| y) else {
            return Err(CcError::UnrecognisedCcFamily);
        };

        let exe_path = exe_path(input);

        let mut extra_args = match family {
            Gnu => vec![format!("-o{}", exe_path)],
            Clang => todo!(),
            Mswc => vec![
                "ucrt.lib".into(),
                "vcruntime.lib".into(),
                format!("-link /ENTRY:{ENTRY_POINT_NAME} /SUBSYSTEM:CONSOLE"),
            ],
        };

        let has_mold = !input.enabled("no-mold")
            && Command::new("mold")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
                .is_ok();

        if has_mold {
            match family {
                Gnu if Path::new("/usr/libexec/mold").exists() => {
                    extra_args.push("-B/usr/libexec/mold".into())
                }
                Clang => extra_args.push("-fuse-ld=mold".into()),
                _ => (),
            }
        }

        let link_with = input.value("link-with").unwrap_or("").split_whitespace();

        let output = compiler
            .to_command()
            .args(extra_args)
            .arg(&obj_path)
            .args(link_with)
            .output()
            .map_err(CcError::CcInvokeFailure)?;

        if !output.status.success() {
            return Err(CcError::LinkingFailed((
                output.status,
                String::from_utf8_lossy(&output.stderr).into(),
            )));
        }

        Ok(exe_path)
    }
}
