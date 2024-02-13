setlocal

set _dir=%~dp0
set _build_dir=%_dir%build

call copy "%_dir%\nop_loop.asm" "%_build_dir%\"
call copy "%_dir%\multi_nop.asm" "%_build_dir%\"

IF NOT EXIST "%_build_dir%" mkdir "%_build_dir%"

call "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"

set msvc_path=C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
IF EXIST "%msvc_path%" SET PATH=%PATH%;"%msvc_path%"

pushd "%_build_dir%"

call nasm -f win64 -o nop_loop.obj nop_loop.asm
call link /dll /export:NOPAllBytesASM /export:MOVAllBytesASM /export:CMPAllBytesASM /export:DECAllBytesASM nop_loop.obj MSVCRTD.lib
call nasm -f win64 -o multi_nop.obj multi_nop.asm
call link /dll /export:NOP3x1AllBytes /export:NOP1x3AllBytes /export:NOP1x9AllBytes multi_nop.obj MSVCRTD.lib

call copy "%_build_dir%\nop_loop.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\multi_nop.dll" "%_dir%haversine\bin\Debug\net8.0\"

popd

endlocal