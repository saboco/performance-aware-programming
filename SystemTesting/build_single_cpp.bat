@echo off

set _build_dir=%2
set _file=%1
set _file_name=%~n1

pushd "%_build_dir%"

where /q cl && (
  call cl -arch:AVX2 -nologo -Zi -W4 -FC "%_file%" -Fe%_file_name%_debug.dll /link /DLL /PDBALTPATH:%_file_name%_debug.pdb
)

popd

endlocal