set TEST_PROJECT_PATH=test_project

mkdir %TEST_PROJECT_PATH%
cd %TEST_PROJECT_PATH%

echo deps { a "a"; b "b" } > package.ctlm

mkdir a
cd a
echo root: "goo.ctl" > package.ctlm
cd ..

mkdir b
cd b
echo root: "moo.ctl" > package.ctlm
cd ../..

cargo test -- --nocapture

rmdir /S /Q %TEST_PROJECT_PATH%