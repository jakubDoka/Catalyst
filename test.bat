mkdir foo
cd foo

git ls-remote https://github.com/rust-lang/rust refs/tags/v1.*.*^^{}

cd ..
rmdir /S /Q foo