setlocal

set _dir=%~dp0
set _build_dir=%_dir%build
set _file=%~dp0rdtsc.cpp

IF NOT EXIST "%_build_dir%" mkdir "%_build_dir%"

setlocal
call "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"

set msvc_path=C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
IF EXIST "%msvc_path%" SET PATH=%PATH%;"%msvc_path%"

pushd "%_build_dir%"
 
call cl -nologo -Zi -FC "%_file%" -Ferdtsc_debug.dll /link /DLL /PDBALTPATH:rdtsc_debug.pdb /export:Rdtsc

call copy "%_dir%\build\rdtsc*.dll" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_dir%\build\rdtsc*.lib" "%_dir%haversine\bin\Debug\net8.0\"
call copy "%_dir%\build\rdtsc*.pdb" "%_dir%haversine\bin\Debug\net8.0\"

popd

endlocal