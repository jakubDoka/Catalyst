#![feature(slice_group_by)]

use std::{env::args, path::PathBuf};

use ast::SourceCode;
use tokens::Lexer;

mod ast;
mod tokens;

fn main() {
    let file_path = args().nth(1).expect("no file given");

    let mut frontier = vec![PathBuf::from(file_path)];

    while let Some(path) = frontier.pop() {
        println!("loading {}", path.display());
        let source = std::fs::read_to_string(&path).expect("could not read file");
        let mut lexer = Lexer::new(&source);
        let ast = SourceCode::new(&mut lexer, path);
        for file in ast.files {
            println!("generating {}", file.path.display());
            std::fs::write(&file.path, format!("{file}")).expect("could not write file");
        }
        frontier.extend(ast.calls.into_iter().map(|call| call.path));
    }
}
