module SystemTesting.System
open System
open System.Runtime.InteropServices

type ProcessorArchitecture =
    | AMD64 = 9us
    | ARM = 5us
    | ARM64 = 12us
    | IA64 = 6us
    | INTEL = 0us
    | UNKNOWN = 0xffffus
    
type ProcessorType =
    | PROCESSOR_INTEL_386 = 386u
    | PROCESSOR_INTEL_486 = 486u
    | PROCESSOR_INTEL_PENTIUM = 586u
    | PROCESSOR_INTEL_IA64 = 2200u
    | PROCESSOR_AMD_X8664 = 8664u
    | PROCESSOR_ARM = 0xffffu

[<Struct;StructLayout(LayoutKind.Sequential)>]
type OemId = { 
    ProcessorArchitecture : ProcessorArchitecture
    Reserved : UInt16
} 
  
[<Struct;StructLayout(LayoutKind.Sequential)>]
type SystemInfo = {
  OemId: OemId
  PageSize: UInt32
  MinimumApplicationAddress: IntPtr
  MaximumApplicationAddress: IntPtr
  ActiveProcessorMask: UIntPtr
  NumberOfProcessors : UInt32
  ProcessorType: ProcessorType
  AllocationGranularity : UInt32
  ProcessorLevel: UInt16
  ProcessorRevision: UInt16
}

let [<Literal>] kernell32 = "kernel32.dll"

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern void GetSystemInfo (SystemInfo* sysemInfo)