setlocal

set _dir=%~dp0
set _build_dir=%_dir%build
set _file=%_dir%windows_metrics.cpp

IF NOT EXIST "%_build_dir%" mkdir "%_build_dir%"

call "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"

set msvc_path=C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
IF EXIST "%msvc_path%" SET PATH=%PATH%;"%msvc_path%"

pushd "%_build_dir%"


call cl -nologo -Zi -FC "%_file%" -Fewindows_metrics_debug.dll /link /DLL /PDBALTPATH:windows_metrics_debug.pdb

call copy "%_dir%build\windows_metrics*.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_dir%build\windows_metrics*.lib" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_dir%build\windows_metrics*.pdb" "%_dir%haversine\bin\Debug\net8.0\"

popd
endlocal