module haversine.Memory
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

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
    | PAGE_EXECUTE_READ=0x20u
    | PAGE_EXECUTE_READWRITE=0x40u
    | PAGE_EXECUTE_WRITECOPY=0x80u
    | PAGE_NOACCESS=0x01u
    | PAGE_READONLY=0x02u
    | PAGE_READWRITE=0x04u
    | PAGE_WRITECOPY=0x08u
    | PAGE_TARGETS_INVALID=0x40000000u
    | PAGE_TARGETS_NO_UPDATE=0x40000000u
    | PAGE_GUARD=0x100u
    | PAGE_NOCACHE=0x200u // The PAGE_NOCACHE flag cannot be used with the PAGE_GUARD, PAGE_NOACCESS, or PAGE_WRITECOMBINE flags.
    | PAGE_WRITECOMBINE=0x400u
    
type FreeType =
    | MEM_DECOMMIT=0x00004000u
    | MEM_RELEASE=0x00008000u
    | MEM_COALESCE_PLACEHOLDERS = 0x00000001u
    | MEM_PRESERVE_PLACEHOLDER = 0x00000002u

[<Flags>]
type WriteTrackingState =
    | WRITE_WATCH_FLAG_RESET = 0x1u
   
module Native =
    let [<Literal>] kernell32 = "kernel32.dll"
    let [<Literal>] kernellbase = "kernelbase.dll"
    
    [<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint VirtualAlloc (nativeint lpAddress, UInt64 size, AllocationType allocationType, MemoryProtection memoryProtection)
    
    [<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
    extern bool VirtualFree (nativeint lpAddress, UInt64 size, FreeType allocationType)
        
    [<Struct;StructLayout(LayoutKind.Sequential)>]
    type ParameterType = {
        Type : UInt64
        Reserved : UInt64
    }
    [<Struct;StructLayout(LayoutKind.Sequential)>]
    type MemExtendedParameter = {
        ParameterType : ParameterType
        ULong64 : UInt64
        Pointer : IntPtr
        Size : IntPtr
        Handle  : IntPtr
        ULong : UInt32
    }
    
    [<DllImport(kernellbase, CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint VirtualAlloc2 (nativeint hProcess, nativeint baseAddress, UInt64 size, AllocationType allocationType, MemoryProtection memoryProtection, MemExtendedParameter* ex, UInt32 parameterCount)
    
    [<DllImport(kernellbase, CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint MapViewOfFile3(
        nativeint fileMapping,
        nativeint hProcess,
        nativeint baseAddress,
        UInt64 offset,
        UInt64 viewSize,
        AllocationType allocationType,
        MemoryProtection pageProtection,
        MemExtendedParameter *ExtendedParameters,
        UInt32 parameterCount)
    
    [<DllImport(kernell32, CallingConvention=CallingConvention.Cdecl)>]
    extern bool UnmapViewOfFile(nativeint lpBaseAddress)
    
    [<Struct;StructLayout(LayoutKind.Sequential)>]
    type  SecurityAttributes = {
          Length : UInt32
          SecurityDescriptor: IntPtr
          InheritHandle: bool
    } 

    [<DllImport(kernell32, CallingConvention=CallingConvention.Cdecl)>]
    extern IntPtr CreateFileMapping(
        IntPtr  hFile,
        UInt32 lpFileMappingAttributes,
        MemoryProtection flProtect,
        UInt32 dwMaximumSizeHigh,
        UInt32 dwMaximumSizeLow,
        UInt32 lpName)
    
    [<DllImport(kernell32, CallingConvention=CallingConvention.Cdecl)>]
    extern bool CloseHandle(IntPtr handle)
    
    [<DllImport(kernell32, CallingConvention=CallingConvention.Cdecl)>]
    extern UInt32 GetWriteWatch(WriteTrackingState flags, IntPtr baseAddress, UInt64 RegionSize, UIntPtr* addresses, UInt64* count, UInt64* lpdwGranularity)
    
    [<DllImport("nop_loop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void MOVAllBytesASM(UInt64 Count, byte* Data)
    
    [<DllImport("nop_loop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOPAllBytesASM(UInt64 Count)
    
    [<DllImport("nop_loop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void CMPAllBytesASM(UInt64 Count)
    
    [<DllImport("nop_loop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void DECAllBytesASM(UInt64 Count)
    
    [<DllImport("multi_nop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOP3x1AllBytes(UInt64 Count, byte* Data)
    
    [<DllImport("multi_nop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOP1x3AllBytes(UInt64 Count, byte* Data)
    
    [<DllImport("multi_nop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOP1x9AllBytes(UInt64 Count, byte* Data)
    
    [<DllImport("conditional_nop.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void ConditionalNOP(UInt64 Count, byte* Data)
    
    [<DllImport("Bcrypt.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern Int32 BCryptGenRandom(int hAlgorithm, byte*  pbBuffer, UInt32 cbBuffer, UInt32 dwFlags)
    
    [<DllImport("jump_alignment.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOPAligned64(UInt64 Count, byte* Data)
    
    [<DllImport("jump_alignment.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOPAligned1(UInt64 Count, byte* Data)
    
    [<DllImport("jump_alignment.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOPAligned15(UInt64 Count, byte* Data)
    
    [<DllImport("jump_alignment.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOPAligned31(UInt64 Count, byte* Data)
    
    [<DllImport("jump_alignment.dll", CallingConvention=CallingConvention.Cdecl)>]
    extern void NOPAligned63(UInt64 Count, byte* Data)
    
open Native

#nowarn "9"

module Buffers =
    [<Struct>]
    type Buffer = {
        mutable Count : UInt64
        mutable Data : nativeptr<byte>
    }
    
    let isInBounds(source : Buffer, at : UInt64) = at < source.Count

    let areEqual(a : Buffer) (b : Buffer) =
        if a.Count <> b.Count then
            false
        else
            let mutable isEqual = true
            let mutable index = 0
            let count = int a.Count
            while index <  count do
                if NativePtr.get a.Data index <> NativePtr.get b.Data index then
                    isEqual <- false
                    index <- count // early break
                
            isEqual 

    let allocateBuffer(count : UInt64) : Buffer =
        let size = unativeint count
        { Count = count; Data = NativeMemory.Alloc size |>  NativePtr.ofVoidPtr  }
        
    let freeBuffer (buffer :Buffer byref) =
        if buffer.Data <> Unchecked.defaultof<nativeptr<byte>> then
            NativeMemory.Free(NativePtr.toVoidPtr buffer.Data)
        
        buffer <- Unchecked.defaultof<Buffer>
        
    let allocateBufferV totalSize =
        let mem = VirtualAlloc(
            IntPtr.Zero,
            uint64 totalSize,
            AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT,
            MemoryProtection.PAGE_READWRITE)
        
        { Count = totalSize; Data = NativePtr.ofNativeInt mem }
        
    let freeBufferV (buffer :Buffer byref) =
        if buffer.Data <> NativePtr.nullPtr then
            VirtualFree(NativePtr.toNativeInt buffer.Data, 0UL, FreeType.MEM_RELEASE) |> ignore
    
        buffer <- Unchecked.defaultof<Buffer>
        
open Buffers

module CirularBuffer =
    let [<Literal>] INVALID_HANDLE_VALUE = -1
    let [<Literal>] NULL = 0
    
    [<Struct>]
    type CircularBuffer ={
        mutable Root : Buffer
        FileMapping: IntPtr
        RepCount : UInt32
    }
    
    let isValid (circularBuffer : CircularBuffer) = circularBuffer.Root.Data <> NativePtr.nullPtr<byte>
    
    let roundToPow2Size (minimumSize : UInt64) (pow2Size : UInt64) =
        (minimumSize + pow2Size - 1UL) &&& ~~~(pow2Size - 1UL)
    
    let unmapCircularBuffer (root:nativeptr<byte>) (size:UInt64) (repCount: UInt32) =
        for repIndex in 0u..(repCount - 1u) do
            let ptr = NativePtr.add root (int (uint64 repIndex * size)) |> NativePtr.toNativeInt
            UnmapViewOfFile(ptr) |> ignore

    let deallocateCircularBuffer (buffer : byref<CircularBuffer> ) =
        if isValid buffer then 
        
            if buffer.FileMapping <> INVALID_HANDLE_VALUE then
                unmapCircularBuffer buffer.Root.Data buffer.Root.Count buffer.RepCount
                CloseHandle buffer.FileMapping |> ignore
            
            buffer <- Unchecked.defaultof<CircularBuffer>
    
    let allocateCircularBuffer(minimumSize : UInt64, repCount : UInt32) : CircularBuffer =
        // NOTE(casey): Allocation size has to be aligned to the allocation granularity otherwise the back-to-back buffer mapping might not work.
        
        let mutable systemInfo = Unchecked.defaultof<SystemInfo>
        GetSystemInfo(&&systemInfo)
        let dataSize = roundToPow2Size minimumSize (uint64 systemInfo.AllocationGranularity)
        let totalRepeatedSize = (uint64 repCount) * dataSize;
        let mutable circularBuffer = {
            Root = Unchecked.defaultof<Buffer>
            FileMapping =
                CreateFileMapping(INVALID_HANDLE_VALUE, 0u, MemoryProtection.PAGE_READWRITE, uint32 (dataSize >>> 32), uint32 (dataSize &&& 0xffffffffUL), 0u)
            RepCount = repCount }
        
        if circularBuffer.FileMapping <> INVALID_HANDLE_VALUE then
            // NOTE(casey): On up-to-date versions of Windows, we can allocate ring buffers directly without
            // needing to hunt for addresses by using "placeholders".
            let mutable memEx = NativePtr.nullPtr<MemExtendedParameter>
            let rootPtr = VirtualAlloc2(0, 0, totalRepeatedSize, AllocationType.MEM_RESERVE||| AllocationType.MEM_RESERVE_PLACEHOLDER, MemoryProtection.PAGE_NOACCESS, memEx, 0u)
            
            let mutable mapped = true
            for repIndex in 0u..(repCount - 1u) do
                let baseAddr = rootPtr + nativeint (uint64 repIndex * dataSize)
                VirtualFree(baseAddr, dataSize, FreeType.MEM_RELEASE ||| FreeType.MEM_PRESERVE_PLACEHOLDER) |> ignore
                if(MapViewOfFile3(circularBuffer.FileMapping, 0, baseAddr, 0UL, dataSize, AllocationType.MEM_REPLACE_PLACEHOLDER, MemoryProtection.PAGE_READWRITE, memEx, 0u) = NULL) then
                    mapped <- false
            
            if mapped then
                circularBuffer.Root.Data <- NativePtr.ofNativeInt rootPtr
                circularBuffer.Root.Count <- dataSize
        
        if circularBuffer.Root.Data = NativePtr.nullPtr<byte> then
            deallocateCircularBuffer(&circularBuffer)
        
        circularBuffer
        
      
    let circularBufferTest () =
        printfn "Circular buffer test:\n"
        
        let mutable circular = allocateCircularBuffer(64UL*4096UL, 3u)
        if isValid circular then
            let size = int32 circular.Root.Count
            let data = NativePtr.add circular.Root.Data size;
            NativePtr.set data 0 123uy 
            
            printfn $"[%8d{-size}]: %u{NativePtr.get data -size}"
            printfn $"[%8d{0}]: %u{NativePtr.get data 0}"
            printfn $"[%8d{size}]: %u{NativePtr.get data size}"
            
            deallocateCircularBuffer(&circular);
        else
            printfn "FAILED"

module TrackedBuffer =
    let [<Literal>] INVALID_HANDLE_VALUE = -1
    let [<Literal>] NULL = 0
    
    [<Struct;StructLayout(LayoutKind.Sequential)>]
    type AddressArray ={
        mutable Addresses : nativeptr<UIntPtr>;
        mutable Count: UInt64
        mutable PageSize: UInt64
    }

    [<Struct>]
    type TrackedBuffer = {
        mutable Base : Buffer
        mutable Results : AddressArray
    }

    let isValid (buffer : TrackedBuffer) = buffer.Base.Data <> NativePtr.nullPtr<byte>

    let deallocateTrackedBuffer(buffer : byref<TrackedBuffer>) =
        if buffer <> Unchecked.defaultof<TrackedBuffer> then
            if buffer.Base.Data <> NativePtr.nullPtr<byte> then
                VirtualFree(NativePtr.toNativeInt buffer.Base.Data, 0UL, FreeType.MEM_RELEASE) |> ignore
            if buffer.Results.Addresses <> NativePtr.nullPtr<UIntPtr> then
                VirtualFree(NativePtr.toNativeInt buffer.Results.Addresses, 0UL, FreeType.MEM_RELEASE) |> ignore
            
            buffer <- Unchecked.defaultof<TrackedBuffer>

    let allocateTrackedBuffer (minimumSize: UInt64) : TrackedBuffer =
        let mutable trackedBuffer = Unchecked.defaultof<TrackedBuffer>
        // NOTE(casey): To make sure we have enough space to store all of the changed pages,
        // we have to ensure we allocate as many entries in our changed address table as there
        // are total pages in the requested buffer size.
        let mutable systemInfo = Unchecked.defaultof<SystemInfo>
        GetSystemInfo(&&systemInfo);
        let pageCount = ((minimumSize + uint64 systemInfo.PageSize - 1UL) / uint64 systemInfo.PageSize);
    
        trackedBuffer.Base.Count <- minimumSize
        
        trackedBuffer.Base.Data <-
            VirtualAlloc(0, minimumSize, AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT ||| AllocationType.MEM_WRITE_WATCH, MemoryProtection.PAGE_READWRITE)
            |> NativePtr.ofNativeInt

        trackedBuffer.Results.Count <- pageCount
        let addr = pageCount * uint64 sizeof<nativeptr<nativeptr<byte>>>
        
        trackedBuffer.Results.Addresses <-
            VirtualAlloc(0, addr, AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT, MemoryProtection.PAGE_READWRITE)
            |> NativePtr.ofNativeInt
        
        if (trackedBuffer.Base.Data = NativePtr.nullPtr<byte>) || (trackedBuffer.Results.Addresses = NativePtr.nullPtr<UIntPtr>) then
           deallocateTrackedBuffer(&trackedBuffer)
        
        trackedBuffer
    
    let getAndResetWrittenPages (buffer : byref<TrackedBuffer>) : AddressArray =
    
        let mutable addressArray = Unchecked.defaultof<AddressArray>
        
        let mutable pageSize = 0UL
        let mutable addressCount : UInt64 = buffer.Results.Count
        if GetWriteWatch(WriteTrackingState.WRITE_WATCH_FLAG_RESET, NativePtr.toNativeInt buffer.Base.Data, buffer.Base.Count, buffer.Results.Addresses, &&addressCount, &&pageSize) = 0u then
            addressArray.Addresses <- buffer.Results.Addresses;
            addressArray.Count <- addressCount;
            addressArray.PageSize <- pageSize;
        
        addressArray
        
    let printAddressArray (written : AddressArray) (baseAddress : nativeptr<byte>) =
        for pageIndex in 0UL..(written.Count - 1UL) do
            let addr = NativePtr.get written.Addresses (int pageIndex) |> uint64
            let offset = (NativePtr.toNativeInt baseAddress) |> uint64 
            printfn $"%i{pageIndex}: {(addr - offset) / written.PageSize}"

    let run() =
        printfn "\nTracked buffer test:"
        
        let mutable tracked = allocateTrackedBuffer(256UL*4096UL)
        
        if isValid(tracked) then
        
            NativePtr.set tracked.Base.Data (15*4096) 1uy
            NativePtr.set tracked.Base.Data (25*4096) 2uy
            NativePtr.set tracked.Base.Data (35*4096) 3uy
            NativePtr.set tracked.Base.Data (45*4096) 4uy
            NativePtr.set tracked.Base.Data (55*4096) 5uy
            
            printf("  --- Pass A ---\n");
            let written = getAndResetWrittenPages(&tracked)
            printAddressArray written tracked.Base.Data
            
            NativePtr.set tracked.Base.Data (11*409) 1uy
            NativePtr.set tracked.Base.Data (11*4096 + 10)  2uy
            NativePtr.set tracked.Base.Data (22*4096 + 291)  3uy
            NativePtr.set tracked.Base.Data (33*4096 + 382)  4uy
            NativePtr.set tracked.Base.Data (44*4096 + 473)  5uy
            NativePtr.set tracked.Base.Data (55*4096 + 948)  6uy
            
            printf("  --- Pass B ---\n");
            let written = getAndResetWrittenPages(&tracked);
            printAddressArray written tracked.Base.Data
            
            deallocateTrackedBuffer(&tracked);
        
        else
            printf("  FAILED\n");
   
module SparseMemory =
    
    [<Struct>]
    type SparseBuffer = {
        mutable Base : Buffer
    }

    let isValid (buffer : SparseBuffer) = buffer.Base.Data <> NativePtr.nullPtr<byte>
    

    let allocateSparseBuffer(size : UInt64) : SparseBuffer =
        
        let mutable sparseBuffer = Unchecked.defaultof<SparseBuffer>
        sparseBuffer.Base.Data <-
            VirtualAlloc(0, size, AllocationType.MEM_RESERVE, MemoryProtection.PAGE_NOACCESS)
            |> NativePtr.ofNativeInt
            
        if isValid sparseBuffer then
            sparseBuffer.Base.Count <- size
        
        sparseBuffer

    let deallocateSparseBuffer(buffer: byref<SparseBuffer>) =
        if isValid buffer then
            VirtualFree(NativePtr.toNativeInt buffer.Base.Data, 0UL, FreeType.MEM_RELEASE) |> ignore
            buffer <- Unchecked.defaultof<SparseBuffer>
    
    let ensureMemoryIsMapped(pointer: nativeptr<byte>) (size : UInt64) =
        VirtualAlloc(NativePtr.toNativeInt pointer, size, AllocationType.MEM_COMMIT, MemoryProtection.PAGE_READWRITE)
        |> ignore
    
    let run() =
        printfn "\nSparse memory test:"

        let gigabyte = 1024UL*1024UL*1024UL
        let mutable sparse = allocateSparseBuffer(256UL*gigabyte)
        if isValid sparse then
        
            let write = sparse.Base.Data
        
            let offsets = [|16UL*gigabyte; 100UL*gigabyte; 200UL*gigabyte; 255UL*gigabyte|]
        
            for offsetIndex in 0..(offsets.Length-1) do
                let mutable write = write
                let offset = offsets[offsetIndex]
                write <- NativePtr.toNativeInt write + nativeint offset  |> NativePtr.ofNativeInt
                ensureMemoryIsMapped write (uint64 sizeof<nativeptr<byte>>)
                NativePtr.write write (byte (100 + offsetIndex))
        
            for offsetIndex in  0..(offsets.Length - 1) do
                let mutable write = write
                let offset = offsets[offsetIndex]
                write <- NativePtr.toNativeInt write +  nativeint offset  |> NativePtr.ofNativeInt
                printfn $"{offsetIndex}: {NativePtr.read write}"
            
        deallocateSparseBuffer(&sparse);

module ReadWriteTests =
    open Diagnostics
    open System.IO
    
    let readAllText path =
        fun () ->
            let content = File.ReadAllText path
            int64 content.Length * 1L<b>

    let readAllBytes path =
        fun () ->
            let bytes = File.ReadAllBytes path
            int64 bytes.Length * 1L<b>
            
    let readAllLines path =
        fun () ->
            let content = File.ReadLines path
            content
            |> Seq.map (fun s-> s.Length)
            |> Seq.reduce (+)
            |> fun count ->
                int64 count * 1L<b>
            
    let writeToAllBytes (capacity: uint64) =
        fun () ->
            let mem = Array.create (int capacity) 1L
            int64 mem.Length * 1L<b>
        
    let writeToAllBytesBuffer (buffer : Buffer) =
        fun () ->
            let count = int buffer.Count - 1
            for i in 0..count do
               NativePtr.set buffer.Data i (byte i)
            int64 buffer.Count * 1L<b>
            
    let MovAllBytesAsm (buffer : Buffer) =
        fun () ->
            MOVAllBytesASM(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
        
    let NOPAllBytesASM (buffer:Buffer) =
        fun () ->
            NOPAllBytesASM(buffer.Count)
            int64 buffer.Count * 1L<b>
            
    let CMPAllBytesASM (buffer:Buffer) =
        fun () ->
            CMPAllBytesASM(buffer.Count)
            int64 buffer.Count * 1L<b>
            
    let DECAllBytesASM (buffer : Buffer) =
        fun () ->
            DECAllBytesASM(buffer.Count)
            int64 buffer.Count * 1L<b>
            
    let NOP3x1AllBytes (buffer : Buffer) =
        fun () ->
            NOP3x1AllBytes(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let NOP1x3AllBytes (buffer : Buffer) =
        fun () ->
            NOP1x3AllBytes(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let NOP1x9AllBytes (buffer : Buffer) =
        fun () ->
            NOP1x9AllBytes(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
    
    type BranchPattern =
        | NeverTaken
        | AlwaysTaken
        | Every2
        | Every3
        | Every4
        | RTRandom
        | OSRandom
        
    let getMaxOSRandomCount() : UInt64 = 0xffffffffUL

    let readOSRandomBytes (count : UInt64) (dest : nativeptr<byte>) =
        let mutable result = false
        if count < getMaxOSRandomCount() then
            let r = BCryptGenRandom(0, dest, uint count, 0x00000002u)
            result <- r = 0
            
        result
    
    let fillWithRandomBytes (dest : Buffer) =
        let maxRandCount = getMaxOSRandomCount()
        let mutable atOffset = 0UL
        while atOffset < dest.Count do
            let mutable readCount = dest.Count - atOffset
            if readCount > maxRandCount then
                readCount <- maxRandCount
            
            let ptr = NativePtr.toNativeInt dest.Data + nativeint atOffset
            
            if not (readOSRandomBytes readCount (NativePtr.ofNativeInt ptr)) then
                failwithf "fail filling buffer with system random bytes"
                
            atOffset <- atOffset + readCount;

    let fillBufferWithPattern (pattern : BranchPattern) (buffer: Buffer) =
        let random = Random()
        let count = int buffer.Count - 1
        match pattern with
        | OSRandom -> fillWithRandomBytes buffer
        | _ ->
            for i in 0..count do
                let value = 
                    match pattern with
                    | NeverTaken -> 0
                    | AlwaysTaken -> 1
                    | Every2 -> if i % 2 = 0 then 1 else 0
                    | Every3 -> if i % 3 = 0 then 1 else 0
                    | Every4 -> if i % 4 = 0 then 1 else 0
                    | RTRandom -> random.Next() 
                    | OSRandom -> 0
                    
                NativePtr.set buffer.Data i (byte value)
            
    let conditionalNop (count : UInt64) (data : nativeptr<byte>) =
        fun () ->
            ConditionalNOP(count, data)
            int64 count * 1L<b>
            
    let runReadWriteTests () =
        let gigabyte = 1024UL*1024UL*1024UL
        let mutable buffer = allocateBufferV gigabyte
        let functions = [|
            "writeToAllBytesBuffer", writeToAllBytesBuffer buffer
            "NOP3x1AllBytes", NOP3x1AllBytes buffer
            "NOP1x3AllBytes", NOP1x3AllBytes buffer
            "NOP1x9AllBytes", NOP1x9AllBytes buffer
        |]
        
        Console.CursorVisible <- false
        while true do
            for name, fn in functions do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"
                
        freeBufferV &buffer
        
    let NOPAligned64 (buffer : Buffer) =
        fun () ->
            NOPAligned64(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let NOPAligned1 (buffer : Buffer) =
        fun () ->
            NOPAligned1(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let NOPAligned15 (buffer : Buffer) =
        fun () ->
            NOPAligned15(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let NOPAligned31 (buffer : Buffer) =
        fun () ->
            NOPAligned63(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let NOPAligned63 (buffer : Buffer) =
        fun () ->
            NOPAligned63(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>
            
    let runJumpAlignments () =
        let gigabyte = 1024UL*1024UL*1024UL
        let mutable buffer = allocateBufferV gigabyte
        let functions = [|
            "NOPAligned64", NOPAligned64 buffer
            "NOPAligned1", NOPAligned1 buffer
            "NOP1x3AllBytes", NOPAligned15 buffer
            "NOP1x9AllBytes", NOPAligned31 buffer
            "NOPAligned63", NOPAligned63 buffer
        |]
        
        Console.CursorVisible <- false
        while true do
            for name, fn in functions do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"
                
        freeBufferV &buffer
            
    let runBranchPatterns () =
        //let path = $"{__SOURCE_DIRECTORY__}\..\..\input\data.json"
        let gigabyte = 1024UL*1024UL*1024UL
        let mutable buffer = allocateBufferV gigabyte
        let cases = 
            [|
                OSRandom  
                NeverTaken
                AlwaysTaken
                Every2
                Every3
                Every4
                RTRandom
            |]
        
        Console.CursorVisible <- false
        while true do
            for case in cases do
                printfn $"--- {case} ---"
                fillBufferWithPattern case buffer
                let fn = conditionalNop (buffer.Count - 10UL) buffer.Data
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"
                
        freeBufferV &buffer
     
module PageFaultTests =
    open Diagnostics
    open System.IO.MemoryMappedFiles
    open System.IO
    
    let touchByInt64 _ touchSize (dPtr : nativeint) =
        let mutable dst : nativeptr<Int64> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
        let mutable index = 0L
        let size = int64 sizeof<Int64>
        while index < touchSize do
            index |> NativePtr.write dst
            dst <- NativePtr.add dst 1
            index <- index + size
       
    let touchByteByByte _ touchSize (dPtr : nativeint) =
        let mutable dst : nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
        let mutable index = 0L
        while index < touchSize do
            (byte index) |> NativePtr.write dst
            dst <- NativePtr.toNativeInt dst + 1n |> NativePtr.ofNativeInt
            index <- index + 1L
            
    let touchByInt64Backwards (totalSize : int64) touchSize (dPtr : nativeint) =
        let mutable dst : nativeptr<Int64> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
        let mutable index = 0L
        let size = int64 sizeof<Int64>
        dst <- NativePtr.add dst (int (totalSize/size))
        dst <- NativePtr.add dst -1
        while index < touchSize do
            index |> NativePtr.write dst
            dst <- NativePtr.add dst -1
            index <- index + size
            
    let touchByteByByteBackwards totalSize touchSize (dPtr : nativeint) =
        let mutable dst : nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
        let mutable index = 0L
        dst <- NativePtr.add dst (int totalSize - 1)
        while index < touchSize do
            (byte index) |> NativePtr.write dst
            dst <- NativePtr.add dst -1 
            index <- index + 1L

    let pageAllocationTest (touchMemory : int64 -> int64 -> nativeint -> unit) (memAlloc : int -> nativeint) (memFree : nativeint -> unit) pageCount =
        Native.InitializeMetrics()
        let pageSize = 4096L
        let totalSize = pageCount * pageSize
        let results = ResizeArray<string>(int pageCount)
        for touchCount in 1L..pageCount do
            let touchSize = pageSize * touchCount
            let dPtr = memAlloc(int totalSize)
            let startFaultCount = Native.ReadPageFaultCount()
            touchMemory totalSize touchSize dPtr
            let endFaultCount = Native.ReadPageFaultCount()
            let faultCount = endFaultCount - startFaultCount
            memFree dPtr
            let addr = VirtualAddress.decomposePointer4K (dPtr + nativeint touchSize - 1n)
            results.Add($"{pageCount};{touchCount};{faultCount};{int64 faultCount - touchCount};{Option.defaultValue 0us addr.DirectoryIndex};{Option.defaultValue 0us addr.TableIndex}")
        
        results
        
    let pageAllocationAnomalyTest printPointer memAlloc memFree (pageCount : uint64) =
        Native.InitializeMetrics()
        let pageSize = 4096UL
        let totalSize = pageCount * pageSize
        let results = ResizeArray<string>(int pageCount)
        let mutable priorOverFaultCount = 0UL
        let mutable priorPageIndex = 0UL
        
        let dPtr : nativeint = memAlloc (int totalSize)
        let mutable dst : nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
        
        let startFaultCount = Native.ReadPageFaultCount()
        let mutable pageIndex = 0UL
        let pageSize = 4096UL
        dst <- NativePtr.add dst (int totalSize - 1) // last byte of the memory
        while pageIndex < pageCount do
            (byte pageIndex) |> NativePtr.write dst
            let endFaultCount = Native.ReadPageFaultCount()
            let overFaultCount = (endFaultCount - startFaultCount) - pageIndex
            if overFaultCount > priorOverFaultCount then
                printfn $"Page %i{pageIndex}: %i{overFaultCount} extra faults (%i{pageIndex - priorPageIndex} pages since last increase)"
                if printPointer then
                    if pageIndex > 0UL then
                        NativePtr.toNativeInt dst + nativeint pageSize
                        |> VirtualAddress.decomposePointer4K  
                        |> VirtualAddress.printAsLine "Previous pointer:"
                    
                    // this pointer
                    NativePtr.toNativeInt dst
                    |> VirtualAddress.decomposePointer4K
                    |> VirtualAddress.printAsLine "    This pointer:"
            
                priorOverFaultCount <- overFaultCount
                priorPageIndex <- pageIndex
                
            dst <- NativePtr.add dst -(int pageSize) // go down by one page
            pageIndex <- pageIndex + 1UL
        
        memFree dPtr
        
        results
        
    let pageAllocationAnomalyTest2 printPointer memAlloc memFree (pageCount : uint64) =
        Native.InitializeMetrics()
        let pageSize = 4096UL
        let totalSize = pageCount * pageSize
        let results = ResizeArray<string>(int pageCount)
        let mutable priorOverFaultCount = 0UL
        let mutable priorPageIndex = 0UL
        
        let dPtr : nativeint = memAlloc (int totalSize)
        let mutable dst : nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
        
        let startFaultCount = Native.ReadPageFaultCount()
        let mutable pageIndex = 0UL
        let pageSize = 4096UL
        dst <- NativePtr.add dst (int totalSize - 1) // last byte of the memory
        while pageIndex < pageCount do
            (byte pageIndex) |> NativePtr.write dst
            let endFaultCount = Native.ReadPageFaultCount()
            let overFaultCount = (endFaultCount - startFaultCount) - pageIndex
            if overFaultCount > priorOverFaultCount then
                printfn $"Page %i{pageIndex}: %i{overFaultCount} extra faults (%i{pageIndex - priorPageIndex} pages since last increase)"
                if printPointer then
                    if pageIndex > 0UL then
                        NativePtr.toNativeInt dst + nativeint pageSize
                        |> VirtualAddress.decomposePointer4K  
                        |> VirtualAddress.printAsLine "Previous pointer:"
                    
                    // this pointer
                    NativePtr.toNativeInt dst
                    |> VirtualAddress.decomposePointer4K
                    |> VirtualAddress.printAsLine "    This pointer:"
            
                priorOverFaultCount <- overFaultCount
                priorPageIndex <- pageIndex
                
            dst <- NativePtr.add dst -(int pageSize) // go down by one page
            pageIndex <- pageIndex + 1UL
        
        memFree dPtr
        
        results
     
    let pageAllocWithAllocHGlobal touchMemory pageCount =
        pageAllocationTest touchMemory Marshal.AllocHGlobal Marshal.FreeHGlobal pageCount
    
    let virtualAlloc totalSize =
        VirtualAlloc(
            IntPtr.Zero,
            uint64 totalSize,
            AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT,
            MemoryProtection.PAGE_READWRITE)
        
    let virtualFree (ptr: nativeint) =
        VirtualFree(ptr, 0UL, FreeType.MEM_RELEASE) |> ignore
            
    let pageAllocationVirtualAlloc touchMemory pageCount =
        pageAllocationTest touchMemory virtualAlloc virtualFree pageCount
        
    let pageAllocWithMemoryMappedFile pageCount =
        Native.InitializeMetrics()
        let pageSize = 4096L
        let totalSize = pageCount * pageSize
        let results = ResizeArray<string>(int pageCount)
        for touchCount in 1L..pageCount do
            let touchSize = pageSize * touchCount
            let mMap = MemoryMappedFile.CreateNew($"page_alloc_test_{touchCount}", int64 totalSize, MemoryMappedFileAccess.ReadWrite)
            let mMapA = mMap.CreateViewAccessor()
            let mutable dst: nativeptr<byte> = NativePtr.nullPtr<byte>
            mMapA.SafeMemoryMappedViewHandle.AcquirePointer(&dst)
            let mutable index = 0L
            let mutable currentByteIndex = 0
            let size = sizeof<Int64>
            let startFaultCount = Native.ReadPageFaultCount()
            while index < touchSize do
                let mutable src : nativeptr<byte> = &&index |> NativePtr.toNativeInt |> NativePtr.ofNativeInt
                while currentByteIndex < size do
                    NativePtr.read src |> NativePtr.write dst
                    dst <- NativePtr.toNativeInt dst + 1n |> NativePtr.ofNativeInt
                    src <- NativePtr.toNativeInt src + 1n |> NativePtr.ofNativeInt
                    currentByteIndex <- currentByteIndex + 1
                index <- index + 1L
            
            mMapA.SafeMemoryMappedViewHandle.ReleasePointer()
            
            let endFaultCount = Native.ReadPageFaultCount()
            let faultCount = endFaultCount - startFaultCount
            results.Add($"{pageCount};{touchCount};{faultCount};{int64 faultCount - touchCount}")
        results

    let executePageFaultProbe (pageCount : UInt64) =
        let results = ResizeArray<string>()
        let putResult (pageCount : UInt64) (touchCount : UInt64) (faultCount : UInt64) (extraFaultCount : UInt64) : unit =
            results.Add($"{pageCount};{touchCount};{faultCount};{extraFaultCount}")
                
        Native.ProbePageFaults (pageCount, Native.PutResult(putResult))
        results
        
    let run () =
        let stamp () = DateTime.Now.ToString("dd_MM_yyyy_HH_mm_ss")
        
        // printfn "Executing pageAllocationAnomalyTest"
        // let results = pageAllocationAnomalyTest true virtualAlloc virtualFree 16384UL
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationAnomalyTest_{stamp ()}.csv", results)
        //
        printfn "Executing pageAllocationVirtualAllocByteByByte"
        let results = pageAllocationVirtualAlloc touchByteByByte 4096L
        File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationVirtualAllocByteByByte_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocationVirtualAllocByteByByteBackwards"
        // let results = pageAllocationVirtualAlloc touchByteByByteBackwards 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationVirtualAllocByteByByteBackwards_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocationVirtualAllocBackWards"
        // let results = pageAllocationVirtualAlloc touchByInt64Backwards 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationVirtualAllocBackWards_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocationVirtualAlloc"
        // let results = pageAllocationVirtualAlloc touchByInt64 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationVirtualAlloc_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocWithAllocHGlobalByteByByte"
        // let results = pageAllocWithAllocHGlobal touchByteByByte 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocWithAllocHGlobalByteByByte_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocWithAllocHGlobalByteByByteBackwards"
        // let results = pageAllocWithAllocHGlobal touchByteByByteBackwards 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocWithAllocHGlobalByteByByteBackwards_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocWithAllocHGlobal"
        // let results = pageAllocWithAllocHGlobal touchByInt64 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocWithAllocHGlobal_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocWithAllocHGlobalBackwards"
        // let results = pageAllocWithAllocHGlobal touchByInt64Backwards 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocWithAllocHGlobalBackwards_{stamp ()}.csv", results)
        //
        // printfn "Executing pageAllocWithMemoryMappedFile"
        // let results = pageAllocWithMemoryMappedFile 4096L
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocWithMemoryMappedFile_{stamp ()}.csv", results)
        //
        // printfn "Executing executePageFaultProbe"
        // let results = executePageFaultProbe 4096UL
        // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\executePageFaultProbe_{stamp ()}.csv", results)
        printfn "Execution end"