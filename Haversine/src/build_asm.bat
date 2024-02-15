setlocal

set _dir=%~dp0
set _build_dir=%_dir%build

call copy "%_dir%\nop_loop.asm" "%_build_dir%\"
call copy "%_dir%\multi_nop.asm" "%_build_dir%\"
call copy "%_dir%\conditional_nop.asm" "%_build_dir%\"
call copy "%_dir%\jump_alignment.asm" "%_build_dir%\"
call copy "%_dir%\rat.asm" "%_build_dir%\"
call copy "%_dir%\read_unroll.asm" "%_build_dir%\"

IF NOT EXIST "%_build_dir%" mkdir "%_build_dir%"

call "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"

set msvc_path=C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
IF EXIST "%msvc_path%" SET PATH=%PATH%;"%msvc_path%"

pushd "%_build_dir%"

call nasm -f win64 -o nop_loop.obj nop_loop.asm
call link /dll /export:NOPAllBytesASM /export:MOVAllBytesASM /export:CMPAllBytesASM /export:DECAllBytesASM nop_loop.obj MSVCRTD.lib

call nasm -f win64 -o multi_nop.obj multi_nop.asm
call link /dll /export:NOP3x1AllBytes /export:NOP1x3AllBytes /export:NOP1x9AllBytes multi_nop.obj MSVCRTD.lib

call nasm -f win64 -o conditional_nop.obj conditional_nop.asm
call link /dll /export:ConditionalNOP conditional_nop.obj MSVCRTD.lib

call nasm -f win64 -o jump_alignment.obj jump_alignment.asm
call link /dll /export:NOPAligned64 /export:NOPAligned1 /export:NOPAligned15 /export:NOPAligned31 /export:NOPAligned63 jump_alignment.obj MSVCRTD.lib

call nasm -f win64 -o rat.obj rat.asm
call link /dll /export:RATAdd /export:RATMovAdd rat.obj MSVCRTD.lib

call nasm -f win64 -o read_unroll.obj read_unroll.asm
call link /dll /export:Read_x1 /export:Read_x2 /export:Read_x3 /export:Read_x4 read_unroll.obj MSVCRTD.lib

call copy "%_build_dir%\nop_loop.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\multi_nop.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\conditional_nop.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\jump_alignment.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\rat.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\read_unroll.dll" "%_dir%haversine\bin\Debug\net8.0\"

popd

endlocal