use cc::*;
use cli::*;
use fmt::*;
use lsp::*;

#[derive(Default)]
struct Catalyst {
    middleware: Middleware,
    cc: CcCtx,
    fmt: FmtRuntimeCtx,
}

impl Catalyst {
    fn run(&mut self) {
        let initial_input = match CliInput::new() {
            Ok(input) => input,
            Err(err) => {
                eprintln!("Cli: {err}");
                return;
            }
        };

        if !initial_input.has_arg(0, "repl") {
            self.dispatch(initial_input);
            return;
        }

        let dialogue = CliDialogue {
            prompt: "=>".into(),
            help: "available commands (+ --help for more info): lsp, cc, fmt".into(),
            exit_word: "qit".into(),
            path: initial_input.wd().into(),
        };

        for input in dialogue {
            self.dispatch(input);
        }
    }

    fn dispatch(&mut self, input: CliInput) {
        let err = match input.args().first().map(|a| &a[..]) {
            Some("cc") => CcRuntime::new(&mut self.middleware, &mut self.cc).run(input),
            Some("lsp") => LspRuntime::immediate(&mut self.middleware).map_err(Into::into),
            Some("fmt") => FmtRuntime::new(&mut self.middleware, &mut self.fmt).run(input),
            Some(inp) => {
                eprintln!("{inp:?} is not a valid command");
                eprintln!("valid commands: lsp, cc, fmt");
                return;
            }
            None => {
                eprintln!("expected subcommand");
                return;
            }
        };

        if let Err(e) = err {
            eprintln!("dispatch error: {e}");
        }
    }
}

fn main() {
    Catalyst::default().run();
}
