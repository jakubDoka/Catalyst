use std::io::{self, Write};

use crate::CliInput;

pub struct CliDialogue {
    pub prompt: String,
    pub help: String,
    pub exit_word: String,
}

impl Iterator for CliDialogue {
    type Item = CliInput;

    fn next(&mut self) -> Option<Self::Item> {
        println!("\n{} write {} to exit", self.prompt, self.exit_word);
        loop {
            eprint!("{} ", self.prompt);
            if let Err(err) = io::stdout().flush() {
                eprintln!("CliDialogueError: could not flush stdout: {err}");
                return None;
            };
            let mut input = String::from("path ");
            if let Err(err) = io::stdin().read_line(&mut input) {
                eprintln!("CliDialogueError: could not read from stdin: {err}");
                return None;
            };
            let input = input.trim();
            if input.is_empty() {
                eprintln!("{}", self.help);
                continue;
            }
            match CliInput::from_string(input) {
                Ok(input) => {
                    if let [arg, ..] = input.args() && arg == &self.exit_word {
                        println!("Exiting...");
                        return None;
                    }
                    print!("{:?} ", input.args());
                    return Some(input);
                }
                Err(e) => println!("CliDialogueError: invalid input: {e}"),
            }
        }
    }
}
