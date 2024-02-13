setlocal

set _dir=%~dp0
set _build_dir=%_dir%build
set _fileName=page_fault_probe
set _file=%_dir%%_fileName%.cpp

IF NOT EXIST "%_build_dir%" mkdir "%_build_dir%"

call "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"

set msvc_path=C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
IF EXIST "%msvc_path%" SET PATH=%PATH%;"%msvc_path%"

pushd "%_build_dir%"

call cl -nologo -Zi -FC "%_file%" -Fe%_fileName%_debug.dll /link /DLL /PDBALTPATH:%_fileName%_debug.pdb

call copy "%_build_dir%\%_fileName%*.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\%_fileName%*.lib" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_build_dir%\%_fileName%*.pdb" "%_dir%haversine\bin\Debug\net8.0\"

popd
endlocal