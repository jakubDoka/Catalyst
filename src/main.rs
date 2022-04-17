fn main() {
    #[cfg(feature = "logging")]
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Trace)
        .init();

    match catalyst_lib::compile() {
        Ok(()) => println!("compiled"),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
