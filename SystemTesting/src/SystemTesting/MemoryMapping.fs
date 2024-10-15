module SystemTesting.MemoryMapping

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Memory.Buffers
open Windows.Native

module CirularBuffer =

    [<Struct>]
    type CircularBuffer =
        { mutable Root: Buffer
          FileMapping: IntPtr
          RepCount: UInt32 }

    let isValid (circularBuffer: CircularBuffer) =
        circularBuffer.Root.Data <> NativePtr.nullPtr<byte>

    let roundToPow2Size (minimumSize: UInt64) (pow2Size: UInt64) =
        (minimumSize + pow2Size - 1UL) &&& ~~~(pow2Size - 1UL)

    let unmapCircularBuffer (root: nativeptr<byte>) (size: UInt64) (repCount: UInt32) =
        for repIndex in 0u .. (repCount - 1u) do
            let ptr = NativePtr.add root (int (uint64 repIndex * size)) |> NativePtr.toNativeInt
            UnmapViewOfFile(ptr) |> ignore

    let deallocateCircularBuffer (buffer: byref<CircularBuffer>) =
        if isValid buffer then

            if buffer.FileMapping <> INVALID_HANDLE_VALUE then
                unmapCircularBuffer buffer.Root.Data buffer.Root.Count buffer.RepCount
                CloseHandle buffer.FileMapping |> ignore

            buffer <- Unchecked.defaultof<CircularBuffer>

    let allocateCircularBuffer (minimumSize: UInt64, repCount: UInt32) : CircularBuffer =
        // NOTE(casey): Allocation size has to be aligned to the allocation granularity otherwise the back-to-back buffer mapping might not work.

        let mutable systemInfo = Unchecked.defaultof<SystemInfo>
        GetSystemInfo(&&systemInfo)
        let dataSize = roundToPow2Size minimumSize (uint64 systemInfo.AllocationGranularity)
        let totalRepeatedSize = (uint64 repCount) * dataSize

        let mutable circularBuffer =
            { Root = Unchecked.defaultof<Buffer>
              FileMapping =
                CreateFileMapping(
                    INVALID_HANDLE_VALUE,
                    0u,
                    MemoryProtection.PAGE_READWRITE,
                    uint32 (dataSize >>> 32),
                    uint32 (dataSize &&& 0xffffffffUL),
                    0u
                )
              RepCount = repCount }

        if circularBuffer.FileMapping <> INVALID_HANDLE_VALUE then
            // NOTE(casey): On up-to-date versions of Windows, we can allocate ring buffers directly without
            // needing to hunt for addresses by using "placeholders".
            let mutable memEx = NativePtr.nullPtr<MemExtendedParameter>

            let rootPtr =
                VirtualAlloc2(
                    0,
                    0,
                    totalRepeatedSize,
                    AllocationType.MEM_RESERVE ||| AllocationType.MEM_RESERVE_PLACEHOLDER,
                    MemoryProtection.PAGE_NOACCESS,
                    memEx,
                    0u
                )

            let mutable mapped = true

            for repIndex in 0u .. (repCount - 1u) do
                let baseAddr = rootPtr + nativeint (uint64 repIndex * dataSize)

                VirtualFree(baseAddr, dataSize, FreeType.MEM_RELEASE ||| FreeType.MEM_PRESERVE_PLACEHOLDER)
                |> ignore

                if
                    (MapViewOfFile3(
                        circularBuffer.FileMapping,
                        0,
                        baseAddr,
                        0UL,
                        dataSize,
                        AllocationType.MEM_REPLACE_PLACEHOLDER,
                        MemoryProtection.PAGE_READWRITE,
                        memEx,
                        0u
                    ) = NULL)
                then
                    mapped <- false

            if mapped then
                circularBuffer.Root.Data <- NativePtr.ofNativeInt rootPtr
                circularBuffer.Root.Count <- dataSize

        if circularBuffer.Root.Data = NativePtr.nullPtr<byte> then
            deallocateCircularBuffer (&circularBuffer)

        circularBuffer


    let circularBufferTest () =
        printfn "Circular buffer test:\n"

        let mutable circular = allocateCircularBuffer (64UL * 4096UL, 3u)

        if isValid circular then
            let size = int32 circular.Root.Count
            let data = NativePtr.add circular.Root.Data size
            NativePtr.set data 0 123uy

            printfn $"[%8d{-size}]: %u{NativePtr.get data -size}"
            printfn $"[%8d{0}]: %u{NativePtr.get data 0}"
            printfn $"[%8d{size}]: %u{NativePtr.get data size}"

            deallocateCircularBuffer (&circular)
        else
            printfn "FAILED"

module TrackedBuffer =
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type AddressArray =
        { mutable Addresses: nativeptr<UIntPtr>
          mutable Count: UInt64
          mutable PageSize: UInt64 }

    [<Struct>]
    type TrackedBuffer =
        { mutable Base: Buffer
          mutable Results: AddressArray }

    let isValid (buffer: TrackedBuffer) =
        buffer.Base.Data <> NativePtr.nullPtr<byte>

    let deallocateTrackedBuffer (buffer: byref<TrackedBuffer>) =
        if buffer <> Unchecked.defaultof<TrackedBuffer> then
            if buffer.Base.Data <> NativePtr.nullPtr<byte> then
                VirtualFree(NativePtr.toNativeInt buffer.Base.Data, 0UL, FreeType.MEM_RELEASE)
                |> ignore

            if buffer.Results.Addresses <> NativePtr.nullPtr<UIntPtr> then
                VirtualFree(NativePtr.toNativeInt buffer.Results.Addresses, 0UL, FreeType.MEM_RELEASE)
                |> ignore

            buffer <- Unchecked.defaultof<TrackedBuffer>

    let allocateTrackedBuffer (minimumSize: UInt64) : TrackedBuffer =
        let mutable trackedBuffer = Unchecked.defaultof<TrackedBuffer>
        // NOTE(casey): To make sure we have enough space to store all of the changed pages,
        // we have to ensure we allocate as many entries in our changed address table as there
        // are total pages in the requested buffer size.
        let mutable systemInfo = Unchecked.defaultof<SystemInfo>
        GetSystemInfo(&&systemInfo)

        let pageCount =
            ((minimumSize + uint64 systemInfo.PageSize - 1UL) / uint64 systemInfo.PageSize)

        trackedBuffer.Base.Count <- minimumSize

        trackedBuffer.Base.Data <-
            VirtualAlloc(
                0,
                minimumSize,
                AllocationType.MEM_RESERVE
                ||| AllocationType.MEM_COMMIT
                ||| AllocationType.MEM_WRITE_WATCH,
                MemoryProtection.PAGE_READWRITE
            )
            |> NativePtr.ofNativeInt

        trackedBuffer.Results.Count <- pageCount
        let addr = pageCount * uint64 sizeof<nativeptr<nativeptr<byte>>>

        trackedBuffer.Results.Addresses <-
            VirtualAlloc(
                0,
                addr,
                AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT,
                MemoryProtection.PAGE_READWRITE
            )
            |> NativePtr.ofNativeInt

        if
            (trackedBuffer.Base.Data = NativePtr.nullPtr<byte>)
            || (trackedBuffer.Results.Addresses = NativePtr.nullPtr<UIntPtr>)
        then
            deallocateTrackedBuffer (&trackedBuffer)

        trackedBuffer

    let getAndResetWrittenPages (buffer: byref<TrackedBuffer>) : AddressArray =

        let mutable addressArray = Unchecked.defaultof<AddressArray>

        let mutable pageSize = 0UL
        let mutable addressCount: UInt64 = buffer.Results.Count

        if
            GetWriteWatch(
                WriteTrackingState.WRITE_WATCH_FLAG_RESET,
                NativePtr.toNativeInt buffer.Base.Data,
                buffer.Base.Count,
                buffer.Results.Addresses,
                &&addressCount,
                &&pageSize
            ) = 0u
        then
            addressArray.Addresses <- buffer.Results.Addresses
            addressArray.Count <- addressCount
            addressArray.PageSize <- pageSize

        addressArray

    let printAddressArray (written: AddressArray) (baseAddress: nativeptr<byte>) =
        for pageIndex in 0UL .. (written.Count - 1UL) do
            let addr = NativePtr.get written.Addresses (int pageIndex) |> uint64
            let offset = (NativePtr.toNativeInt baseAddress) |> uint64
            printfn $"%i{pageIndex}: {(addr - offset) / written.PageSize}"

    let run () =
        printfn "\nTracked buffer test:"

        let mutable tracked = allocateTrackedBuffer (256UL * 4096UL)

        if isValid (tracked) then

            NativePtr.set tracked.Base.Data (15 * 4096) 1uy
            NativePtr.set tracked.Base.Data (25 * 4096) 2uy
            NativePtr.set tracked.Base.Data (35 * 4096) 3uy
            NativePtr.set tracked.Base.Data (45 * 4096) 4uy
            NativePtr.set tracked.Base.Data (55 * 4096) 5uy

            printf ("  --- Pass A ---\n")
            let written = getAndResetWrittenPages (&tracked)
            printAddressArray written tracked.Base.Data

            NativePtr.set tracked.Base.Data (11 * 409) 1uy
            NativePtr.set tracked.Base.Data (11 * 4096 + 10) 2uy
            NativePtr.set tracked.Base.Data (22 * 4096 + 291) 3uy
            NativePtr.set tracked.Base.Data (33 * 4096 + 382) 4uy
            NativePtr.set tracked.Base.Data (44 * 4096 + 473) 5uy
            NativePtr.set tracked.Base.Data (55 * 4096 + 948) 6uy

            printf ("  --- Pass B ---\n")
            let written = getAndResetWrittenPages (&tracked)
            printAddressArray written tracked.Base.Data

            deallocateTrackedBuffer (&tracked)

        else
            printf ("  FAILED\n")

module SparseMemory =

    [<Struct>]
    type SparseBuffer = { mutable Base: Buffer }

    let isValid (buffer: SparseBuffer) =
        buffer.Base.Data <> NativePtr.nullPtr<byte>


    let allocateSparseBuffer (size: UInt64) : SparseBuffer =

        let mutable sparseBuffer = Unchecked.defaultof<SparseBuffer>

        sparseBuffer.Base.Data <-
            VirtualAlloc(0, size, AllocationType.MEM_RESERVE, MemoryProtection.PAGE_NOACCESS)
            |> NativePtr.ofNativeInt

        if isValid sparseBuffer then
            sparseBuffer.Base.Count <- size

        sparseBuffer

    let deallocateSparseBuffer (buffer: byref<SparseBuffer>) =
        if isValid buffer then
            VirtualFree(NativePtr.toNativeInt buffer.Base.Data, 0UL, FreeType.MEM_RELEASE)
            |> ignore

            buffer <- Unchecked.defaultof<SparseBuffer>

    let ensureMemoryIsMapped (pointer: nativeptr<byte>) (size: UInt64) =
        VirtualAlloc(NativePtr.toNativeInt pointer, size, AllocationType.MEM_COMMIT, MemoryProtection.PAGE_READWRITE)
        |> ignore

    let run () =
        printfn "\nSparse memory test:"

        let gigabyte = 1024UL * 1024UL * 1024UL
        let mutable sparse = allocateSparseBuffer (256UL * gigabyte)

        if isValid sparse then

            let write = sparse.Base.Data

            let offsets =
                [| 16UL * gigabyte; 100UL * gigabyte; 200UL * gigabyte; 255UL * gigabyte |]

            for offsetIndex in 0 .. (offsets.Length - 1) do
                let mutable write = write
                let offset = offsets[offsetIndex]
                write <- NativePtr.toNativeInt write + nativeint offset |> NativePtr.ofNativeInt
                ensureMemoryIsMapped write (uint64 sizeof<nativeptr<byte>>)
                NativePtr.write write (byte (100 + offsetIndex))

            for offsetIndex in 0 .. (offsets.Length - 1) do
                let mutable write = write
                let offset = offsets[offsetIndex]
                write <- NativePtr.toNativeInt write + nativeint offset |> NativePtr.ofNativeInt
                printfn $"{offsetIndex}: {NativePtr.read write}"

        deallocateSparseBuffer (&sparse)
