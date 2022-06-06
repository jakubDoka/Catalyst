use std::path::{Path, PathBuf};

use crate::exit;
use cli::*;
use storage::*;

/// Sub command handles parsing of command line arguments
/// for all subcommands.
pub enum Subcommand {
    Compile(PathBuf),
    None,
}

const SUBCOMMANDS: &'static str = "c";

impl Subcommand {
    /// Builds a subcommand based of command line input.
    pub fn new(input: &CmdInput) -> Self {
        let Some(subcommand) = input.args().get(0) else {
            println!("usage: catalyst {INFO}<sub>{END} ...");
            println!("options: {INFO}{SUBCOMMANDS}{END}");
            exit!();
        };

        match subcommand.as_str() {
            "c" => {
                let path = input.args().get(1).map(String::as_str).unwrap_or(".");
                return Self::Compile(PathBuf::from(path));
            }
            sub => {
                println!("invalid subcommand: {sub}");
                println!("options: {INFO}{SUBCOMMANDS}{END}");
                exit!();
            }
        }
    }

    /// Returns a path to the input project.
    pub fn root_path(&self) -> Option<&Path> {
        match self {
            Self::Compile(path) => Some(path),
            _ => None,
        }
    }
}
