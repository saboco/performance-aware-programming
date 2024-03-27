@echo off
setlocal

set _build_dir=%2
set _asm_file=%1
set _output_file=%_build_dir%\%~n1.obj

pushd "%_build_dir%"

where /q nasm && (
  call nasm -f win64 -o %_output_file% %_asm_file%
  call link /dll %_output_file% MSVCRTD.lib
)

popd

endlocal