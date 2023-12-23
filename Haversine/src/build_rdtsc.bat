IF NOT EXIST build mkdir build
pushd build
REM C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x64
call cl -nologo -Zi -FC ..\rdtsc.cpp -Ferdtsc_debug.dll /link /DLL /PDBALTPATH:rdtsc_debug.pdb /export:Rdtsc

call copy rdtsc*.dll ..\haversine\bin\Debug\net7.0\
call copy rdtsc*.lib ..\haversine\bin\Debug\net7.0\
call copy rdtsc*.pdb ..\haversine\bin\Debug\net7.0\

popd