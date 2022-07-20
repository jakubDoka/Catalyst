set PICK=%1

if "%PICK%"=="" (
    set PICK=ALL
)

cd tests

for /D %%x in (.\*) do (
    set PROCESS=false
    if "%PICK%"=="ALL" set PROCESS=true
    if "%PICK%"==".\%%x" set PROCESS=true
    if exist "%%x\*" if "%PROCESS%"=="true" (
        cd %%x
        cargo run
        if exist "test_out\*" (
            cd test_out
            for %%f in (.\*) do (
                git diff %%f
            )
            cd ..
        )
        cd ..
    )   
)

cd ..