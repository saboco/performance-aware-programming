setlocal

set _dir=%~dp0

call "%_dir%build_rdtsc.bat" 
call "%_dir%build_windows_metrics.bat" 
call "%_dir%build_page_fault_probe.bat"
call "%_dir%build_asm.bat"