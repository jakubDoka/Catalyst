@echo off

cargo fmt
git add .
git commit -m%1
git push