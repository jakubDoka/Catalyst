set TEST_PROJECT_PATH=test_project

mkdir %TEST_PROJECT_PATH%
cd %TEST_PROJECT_PATH%

echo deps { a "a"; b "b" } > package.ctlm
echo use { a "a" } > root.ctl

mkdir a
cd a
echo use { water "water" } > goo.ctl
echo root: "goo.ctl"; deps { water git "github.com/jakubDoka/water" "v0.*.*" } > package.ctlm
cd ..

mkdir b
cd b
echo use {} > moo.ctl
echo root: "moo.ctl" > package.ctlm
cd ../..

cargo test -- --nocapture

rmdir /S /Q %TEST_PROJECT_PATH%