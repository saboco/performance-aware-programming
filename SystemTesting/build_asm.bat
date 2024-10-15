@echo off

where /q nasm || (echo "WARNING: nasm not found -- ASM libs wil not be built")

echo "Building asm files..."
setlocal

set _dir=%~dp0
set _asm_files=%_dir%\src\*%1*.asm
set _build_dir=%~2

if "%~1"=="" set _asm_files=%_dir%src\*.asm
for %%G in (%_asm_files%) DO (call %_dir%\build_single_asm.bat %%G "%_build_dir%")

endlocal

