@echo off
where /q cargo
IF ERRORLEVEL 1 (
    ECHO Cargo is not installed, you can install it from `https://www.rust-lang.org/tools/install`.
    EXIT /B
)

set STATE_GEN="target\release\state.exe"

where /q %STATE_GEN%
if ERRORLEVEL 1 (
    cd utils/state
    cargo build --release
    cd ../..
)

%STATE_GEN% state_gen.rbg

