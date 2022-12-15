use cc::*;
use cli::*;
use lsp::*;

fn main() {
    let input = match CliInput::new() {
        Ok(input) => input,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };

    match input.args().first().map(|s| s.as_str()) {
        Some("cc") => CcRuntime::new(input).run(),
        Some("lsp") => match LspRuntime::immediate() {
            Ok(..) => (),
            Err(err) => {
                eprintln!("LspRuntimeError: {err}");
            }
        },
        _ => eprintln!("invalid subcommand"),
    }
}
