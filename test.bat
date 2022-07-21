@echo off
SetLocal EnableDelayedExpansion
set PICK=%1

if "%PICK%"=="" (
    set PICK=ALL
)

cd tests

for /D %%x in (.\*) do (
    set P=false
    if "%PICK%"=="ALL" set P=true
    if ".\%PICK%"=="%%x" set P=true
    if exist "%%x\*" if "!P!"=="true" (
        cd %%x
        cargo run
        if exist "test_out\*" (
            cd test_out
            for %%f in (.\*) do (
                git ls-files --error-unmatch %%f
                if ERRORLEVEL 1 (
                    git add %%f
                    git commit -m "Add %%f"
                )
                git diff --exit-code %%f
                if ERRORLEVEL 1 (
                    choice /C YN /M "Commit new test results?"
                    if not ERRORLEVEL 2 (
                        git add %%f
                        git commit -m "test update"
                    )
                )
            )
            cd ..
        )
        cd ..
    )   
)

cd ..