@echo off

where /q cl || (echo "WARNING: MSVC (cl) not found -- cpp libs wil not be built")

echo "Building cpp files.."

setlocal

set _dir=%~dp0
set _cpp_files=%_dir%\src\*%1*.cpp
set _build_dir=%2

if "%~1"=="" set _cpp_files=%_dir%\src\*.cpp
for %%G in (%_cpp_files%) DO call %_dir%\build_single_cpp.bat %%G %_build_dir%

endlocal

