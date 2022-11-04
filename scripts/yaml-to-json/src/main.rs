#![feature(iter_intersperse)]

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let [_, source, dest] = args.as_slice() else {
        println!("Usage: yaml-to-json <source> <dest>");
        return;
    };

    let source = std::fs::read_to_string(source).unwrap();
    let dest = std::fs::File::create(dest).unwrap();

    if source.ends_with(".json") {
        let emitter: serde_json::Value = serde_json::from_str(&source).unwrap();
        serde_yaml::to_writer(dest, &emitter).unwrap();
    } else {
        let emitter: serde_yaml::Value = serde_yaml::from_str(&source).unwrap();
        serde_json::to_writer_pretty(dest, &emitter).unwrap();
    }
}
