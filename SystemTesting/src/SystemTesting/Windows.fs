module SystemTesting.Windows.Native

open System
open System.Runtime.InteropServices

[<Literal>]
let kernell32 = "kernel32.dll"

[<Literal>]
let kernellbase = "kernelbase.dll"
    
[<Literal>]
let INVALID_HANDLE_VALUE = -1

[<Literal>]
let NULL = 0

[<Flags>]
type AllocationType =
    | MEM_COMMIT = 0x00001000u
    | MEM_RESERVE = 0x00002000u
    | MEM_REPLACE_PLACEHOLDER = 0x00004000u
    | MEM_RESERVE_PLACEHOLDER = 0x00040000u
    | MEM_RESET = 0x00080000u
    | MEM_RESET_UNDO = 0x1000000u
    | MEM_LARGE_PAGES = 0x20000000u // If you specify this value, you must also specify MEM_RESERVE and MEM_COMMIT.
    | MEM_PHYSICAL = 0x00400000u // This value must be used with MEM_RESERVE and no other values.
    | MEM_TOP_DOWN = 0x00100000u
    | MEM_WRITE_WATCH = 0x00200000u // If you specify this value, you must also specify MEM_RESERVE.

[<Flags>]
type MemoryProtection =
    | PAGE_EXECUTE = 0x10u
    | PAGE_EXECUTE_READ = 0x20u
    | PAGE_EXECUTE_READWRITE = 0x40u
    | PAGE_EXECUTE_WRITECOPY = 0x80u
    | PAGE_NOACCESS = 0x01u
    | PAGE_READONLY = 0x02u
    | PAGE_READWRITE = 0x04u
    | PAGE_WRITECOPY = 0x08u
    | PAGE_TARGETS_INVALID = 0x40000000u
    | PAGE_TARGETS_NO_UPDATE = 0x40000000u
    | PAGE_GUARD = 0x100u
    | PAGE_NOCACHE = 0x200u // The PAGE_NOCACHE flag cannot be used with the PAGE_GUARD, PAGE_NOACCESS, or PAGE_WRITECOMBINE flags.
    | PAGE_WRITECOMBINE = 0x400u
    | SEC_COMMIT = 0x8000000u
    | SEC_IMAGE = 0x1000000u
    | SEC_IMAGE_NO_EXECUTE = 0x11000000u
    | SEC_LARGE_PAGES = 0x80000000u
    | SEC_NOCACHE = 0x10000000u
    | SEC_RESERVE = 0x4000000u
    | SEC_WRITECOMBINE = 0x40000000u

type FreeType =
    | MEM_DECOMMIT = 0x00004000u
    | MEM_RELEASE = 0x00008000u
    | MEM_COALESCE_PLACEHOLDERS = 0x00000001u
    | MEM_PRESERVE_PLACEHOLDER = 0x00000002u

[<Flags>]
type WriteTrackingState =
    | WRITE_WATCH_FLAG_RESET = 0x1u
    
[<Flags>]
type GenericAccessRights =
    | GENERIC_ALL = 0x10000000u
    | GENERIC_EXECUTE = 0x20000000u
    | GENERIC_WRITE = 0x40000000u
    | GENERIC_READ = 0x80000000u

[<Flags>]
type FileShareMode =
    | FILE_SHARE_READ = 0x00000001u
    | FILE_SHARE_WRITE = 0x00000002u
    | FILE_SHARE_DELETE = 0x00000004u
    
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
    
type CreationDisposition =
    | CREATE_ALWAYS = 2u
    | CREATE_NEW = 1u
    | OPEN_ALWAYS = 4u
    | OPEN_EXISTING = 3u
    | TRUNCATE_EXISTING = 5u

[<Flags>]
type FileAttributesAndFlags =
    | FILE_ATTRIBUTE_ARCHIVE = 0x20u
    | FILE_ATTRIBUTE_ENCRYPTED = 0x4000u
    | FILE_ATTRIBUTE_HIDDEN = 0x2u
    | FILE_ATTRIBUTE_NORMAL = 0x80u
    | FILE_ATTRIBUTE_OFFLINE = 0x1000u
    | FILE_ATTRIBUTE_READONLY = 0x1u
    | FILE_ATTRIBUTE_SYSTEM = 0x4u
    | FILE_ATTRIBUTE_TEMPORARY = 0x100u
    | FILE_FLAG_BACKUP_SEMANTICS = 0x02000000u
    | FILE_FLAG_DELETE_ON_CLOSE = 0x04000000u
    | FILE_FLAG_NO_BUFFERING = 0x20000000u
    | FILE_FLAG_OPEN_NO_RECALL = 0x00100000u
    | FILE_FLAG_OPEN_REPARSE_POINT = 0x00200000u
    | FILE_FLAG_OVERLAPPED = 0x40000000u
    | FILE_FLAG_POSIX_SEMANTICS = 0x0100000u
    | FILE_FLAG_RANDOM_ACCESS = 0x10000000u
    | FILE_FLAG_SESSION_AWARE = 0x00800000u
    | FILE_FLAG_SEQUENTIAL_SCAN = 0x08000000u
    | FILE_FLAG_WRITE_THROUGH = 0x80000000u
    
[<Flags>]
type FileMapAccess =
    | FILE_MAP_ALL_ACCESS = 0x000f001fu
    | FILE_MAP_READ = 0x0004u
    | FILE_MAP_WRITE = 0x0002u
    | FILE_MAP_COPY = 0x0001u
    | FILE_MAP_EXECUTE = 0x0020u
    | FILE_MAP_LARGE_PAGES = 0x20000000u
    | FILE_MAP_TARGETS_INVALID = 0x40000000u
    
    
[<Struct; StructLayout(LayoutKind.Sequential)>]
type OemId =
    { ProcessorArchitecture: ProcessorArchitecture
      Reserved: UInt16 }

[<Struct; StructLayout(LayoutKind.Sequential)>]
type SystemInfo =
    { OemId: OemId
      PageSize: UInt32
      MinimumApplicationAddress: IntPtr
      MaximumApplicationAddress: IntPtr
      ActiveProcessorMask: UIntPtr
      NumberOfProcessors: UInt32
      ProcessorType: ProcessorType
      AllocationGranularity: UInt32
      ProcessorLevel: UInt16
      ProcessorRevision: UInt16 }

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint VirtualAlloc(
    nativeint lpAddress,
    UInt64 size,
    AllocationType allocationType,
    MemoryProtection memoryProtection
)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern bool VirtualFree(nativeint lpAddress, UInt64 size, FreeType allocationType)

[<Struct; StructLayout(LayoutKind.Sequential)>]
type ParameterType = { Type: UInt64; Reserved: UInt64 }

[<Struct; StructLayout(LayoutKind.Sequential)>]
type MemExtendedParameter =
    { ParameterType: ParameterType
      ULong64: UInt64
      Pointer: IntPtr
      Size: IntPtr
      Handle: IntPtr
      ULong: UInt32 }

type HANDLE = IntPtr
type LPSECURITY_ATTRIBUTES = UInt32 // it can be an actual pointer to SECURITY_ATTRIBUTES struct. cf https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/aa379560(v=vs.85)
type DWORD = UInt32
type LPCSTR = IntPtr
type BOOL = UInt32
type SIZE_T = UInt64
type ULONG64 = UInt64
type LPVOID = IntPtr
type PVOID = IntPtr
type LPCVOID = IntPtr

let [<Literal>] FALSE : BOOL = 0u

[<DllImport(kernellbase, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint VirtualAlloc2(
    nativeint hProcess,
    nativeint baseAddress,
    UInt64 size,
    AllocationType allocationType,
    MemoryProtection memoryProtection,
    MemExtendedParameter* ex,
    UInt32 parameterCount
)
[<DllImport(kernellbase, CallingConvention = CallingConvention.Cdecl)>]
extern LPVOID MapViewOfFile(
  HANDLE hFileMappingObject,
  FileMapAccess  dwDesiredAccess,
  DWORD  dwFileOffsetHigh,
  DWORD  dwFileOffsetLow,
  SIZE_T dwNumberOfBytesToMap
)

[<DllImport(kernellbase, CallingConvention = CallingConvention.Cdecl)>]
extern PVOID MapViewOfFile3(
    HANDLE fileMapping,
    HANDLE hProcess,
    PVOID baseAddress,
    ULONG64 offset,
    SIZE_T viewSize,
    AllocationType allocationType,
    MemoryProtection pageProtection,
    MemExtendedParameter* ExtendedParameters,
    UInt32 parameterCount
)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern BOOL UnmapViewOfFile(LPCVOID lpBaseAddress)

[<Struct; StructLayout(LayoutKind.Sequential)>]
type SecurityAttributes =
    { Length: UInt32
      SecurityDescriptor: IntPtr
      InheritHandle: bool }

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern HANDLE CreateFileMapping(
    HANDLE hFile,
    LPSECURITY_ATTRIBUTES lpFileMappingAttributes,
    MemoryProtection flProtect,
    DWORD dwMaximumSizeHigh,
    DWORD dwMaximumSizeLow,
    LPCSTR lpName
)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern HANDLE CreateFileMappingA(
    HANDLE hFile,
    LPSECURITY_ATTRIBUTES lpFileMappingAttributes,
    MemoryProtection flProtect,
    DWORD dwMaximumSizeHigh,
    DWORD dwMaximumSizeLow,
    LPCSTR lpName
)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern bool CloseHandle(IntPtr handle)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern UInt32 GetWriteWatch(
    WriteTrackingState flags,
    IntPtr baseAddress,
    UInt64 RegionSize,
    UIntPtr* addresses,
    UInt64* count,
    UInt64* lpdwGranularity
)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern void GetSystemInfo(SystemInfo* sysemInfo)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern HANDLE CreateFileA(
    LPCSTR lpFileName,
    GenericAccessRights dwDesiredAccess,
    FileShareMode dwShareMode,
    LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    CreationDisposition dwCreationDisposition,
    FileAttributesAndFlags dwFlagsAndAttributes,
    HANDLE hTemplateFile
)

[<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
extern BOOL ReadFile(
    HANDLE hFile,
    nativeint lpBuffer,
    UInt32 nNumberOfBytesToRead,
    UInt32* lpNumberOfBytesRead,
    nativeint lpOverlapped
)
