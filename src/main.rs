fn main() {
    #[cfg(feature = "logging")]
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Trace)
        .init();

    catalyst_lib::compile()
}
