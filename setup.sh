STATE_GEN="./target/release/state"

TESTER="./target/release/tester"

cd scripts/tester
cargo build --release
cd ../..

CONVERTER="./target/release/yaml-to-json"

cd scripts/yaml-to-json
cargo build --release
cd ../..

mv "$CONVERTER" subcommands/lsp/vscode/catalyst-lsp

# "$STATE_GEN" state_gen.rbg
# "$TESTER" "$1"
