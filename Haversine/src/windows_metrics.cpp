#include <intrin.h>
#include <windows.h>
#include <psapi.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int32_t b32;

typedef float f32;
typedef double f64;

#define DllExport extern "C" __declspec(dllexport)

struct os_metrics
{
    __int32 Initialized;
    HANDLE ProcessHandle;
};
static os_metrics GlobalMetrics;

DllExport u64 __stdcall ReadPageFaultCount(void)
{
    PROCESS_MEMORY_COUNTERS_EX MemoryCounters = {};
    MemoryCounters.cb = sizeof(MemoryCounters);
    GetProcessMemoryInfo(GlobalMetrics.ProcessHandle, (PROCESS_MEMORY_COUNTERS *)&MemoryCounters, sizeof(MemoryCounters));
    
    u64 Result = MemoryCounters.PageFaultCount;
    return Result;
}

DllExport void __stdcall InitializeMetrics(void)
{
    if(!GlobalMetrics.Initialized)
    {
        GlobalMetrics.Initialized = true;
        GlobalMetrics.ProcessHandle = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId());
    }
}