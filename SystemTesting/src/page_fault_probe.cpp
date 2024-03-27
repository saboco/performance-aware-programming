#include <stdio.h>

#include "windows_metrics.cpp"

DllExport void __stdcall ProbePageFaults(u64 pageCount, void (*putResult)(u64,u64,u64,u64))
{
    InitializeMetrics();

    u64 PageSize = 4096;
    u64 PageCount = pageCount;
    u64 TotalSize = PageSize*PageCount;
   
    for(u64 TouchCount = 0; TouchCount <= PageCount; ++TouchCount)
    {
        u64 TouchSize = PageSize*TouchCount;
        u8 *Data = (u8 *)VirtualAlloc(0, TotalSize, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
        if(Data)
        {
            u64 StartFaultCount = ReadPageFaultCount();
            for(u64 Index = 0; Index < TouchSize; ++Index)
            {
                Data[Index] = (u8)Index;
            }
            u64 EndFaultCount = ReadPageFaultCount();
            
            u64 FaultCount = EndFaultCount - StartFaultCount;
            
            putResult(PageCount, TouchCount, FaultCount, (FaultCount - TouchCount));

            VirtualFree(Data, 0, MEM_RELEASE);
        }
        else
        {
            fprintf(stderr, "ERROR: Unable to allocate memory\n");
        }
    }    
}