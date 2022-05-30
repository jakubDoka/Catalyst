@echo off
cargo build --release
move "target\release\state.exe" ..