fn main() {
    match catalyst_lib::compile() {
        Ok(()) => println!("compiled"),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
