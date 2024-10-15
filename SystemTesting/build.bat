@echo off
setlocal

echo "Building external files..."

call "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"

set msvc_path=C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
set _dir=%~dp0
set _files=%_dir%\build_asm.bat,%_dir%\build_cpp.bat
set _build_dir=%_dir%build

IF NOT EXIST "%_build_dir%" mkdir "%_build_dir%"
IF EXIST "%msvc_path%" SET PATH=%PATH%;"%msvc_path%"

FOR %%G IN (%_files%) DO call %%G "" "%_build_dir%"

set _dll_files=%_build_dir%\*dll
set _release_dir=%_dir%\release
set _debug_dir=%_dir%\

IF NOT EXIST "%_release_dir%" mkdir "%_release_dir%"

FOR %%G IN ("%_dll_files%") DO (
    copy %%G "%_release_dir%\"
    copy %%G "%_dir%\src\SystemTesting\bin\Debug\net8.0\"
)

endlocal
