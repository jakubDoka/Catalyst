@echo off
where /q cargo
IF ERRORLEVEL 1 (
    ECHO Cargo is not installed, you can install it from `https://www.rust-lang.org/tools/install`.
    EXIT /B
)

set STATE_GEN="target\release\state.exe"

cd scripts/state
cargo build --release
cd ../..

set TESTER="target\release\tester.exe"

cd scripts/tester
cargo build --release
cd ../..

set CONVERTER="target\release\yaml-to-json.exe"

cd scripts/yaml-to-json
cargo build --release
cd ../..

move %CONVERTER% subcommands\lsp\vscode\catalyst-lsp

%STATE_GEN% state_gen.rbg
%TESTER% %1