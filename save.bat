@echo off

cargo fmt
git add -- . :!tests/*/test_out/*
git commit -m%1
git push