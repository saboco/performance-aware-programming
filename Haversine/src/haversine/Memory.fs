module haversine.Memory

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Plotly.NET

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

type FreeType =
    | MEM_DECOMMIT = 0x00004000u
    | MEM_RELEASE = 0x00008000u
    | MEM_COALESCE_PLACEHOLDERS = 0x00000001u
    | MEM_PRESERVE_PLACEHOLDER = 0x00000002u

[<Flags>]
type WriteTrackingState =
    | WRITE_WATCH_FLAG_RESET = 0x1u

module Native =
    [<Literal>]
    let kernell32 = "kernel32.dll"

    [<Literal>]
    let kernellbase = "kernelbase.dll"

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
    extern nativeint MapViewOfFile3(
        nativeint fileMapping,
        nativeint hProcess,
        nativeint baseAddress,
        UInt64 offset,
        UInt64 viewSize,
        AllocationType allocationType,
        MemoryProtection pageProtection,
        MemExtendedParameter* ExtendedParameters,
        UInt32 parameterCount
    )

    [<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
    extern bool UnmapViewOfFile(nativeint lpBaseAddress)

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type SecurityAttributes =
        { Length: UInt32
          SecurityDescriptor: IntPtr
          InheritHandle: bool }

    [<DllImport(kernell32, CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr CreateFileMapping(
        IntPtr hFile,
        UInt32 lpFileMappingAttributes,
        MemoryProtection flProtect,
        UInt32 dwMaximumSizeHigh,
        UInt32 dwMaximumSizeLow,
        UInt32 lpName
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

    [<DllImport("nop_loop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void MOVAllBytesASM(UInt64 Count, byte* Data)

    [<DllImport("nop_loop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOPAllBytesASM(UInt64 Count)

    [<DllImport("nop_loop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void CMPAllBytesASM(UInt64 Count)

    [<DllImport("nop_loop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void DECAllBytesASM(UInt64 Count)

    [<DllImport("multi_nop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOP3x1AllBytes(UInt64 Count, byte* Data)

    [<DllImport("multi_nop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOP1x3AllBytes(UInt64 Count, byte* Data)

    [<DllImport("multi_nop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOP1x9AllBytes(UInt64 Count, byte* Data)

    [<DllImport("conditional_nop.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void ConditionalNOP(UInt64 Count, byte* Data)

    [<DllImport("Bcrypt.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern Int32 BCryptGenRandom(int hAlgorithm, byte* pbBuffer, UInt32 cbBuffer, UInt32 dwFlags)

    [<DllImport("jump_alignment.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOPAligned64(UInt64 Count, byte* Data)

    [<DllImport("jump_alignment.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOPAligned1(UInt64 Count, byte* Data)

    [<DllImport("jump_alignment.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOPAligned15(UInt64 Count, byte* Data)

    [<DllImport("jump_alignment.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOPAligned31(UInt64 Count, byte* Data)

    [<DllImport("jump_alignment.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void NOPAligned63(UInt64 Count, byte* Data)

    [<DllImport("rat.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void RATAdd()

    [<DllImport("rat.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void RATMovAdd()

    [<DllImport("read_unroll.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void Read_x1(UInt64 Count, byte* Data)

    [<DllImport("read_unroll.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void Read_x2(UInt64 Count, byte* Data)

    [<DllImport("read_unroll.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void Read_x3(UInt64 Count, byte* Data)

    [<DllImport("read_unroll.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void Read_x4(UInt64 Count, byte* Data)

    [<DllImport("byte_read.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void Read_1x2(UInt64 Count, byte* Data)

    [<DllImport("byte_read.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void Read_8x2(UInt64 Count, byte* Data)

    module SIMD =
        [<DllImport("read_withs.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_4x2(UInt64 Count, byte* Data)

        [<DllImport("read_withs.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_8x2(UInt64 Count, byte* Data)

        [<DllImport("read_withs.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_16x2(UInt64 Count, byte* Data)

        [<DllImport("read_withs.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_32x2(UInt64 Count, byte* Data)

        [<DllImport("cache_tests.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_Chunk_x4(UInt64 Count, byte* Data, Int64 mask)

        [<DllImport("cache_tests.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_Chunk_x8(UInt64 Count, byte* Data, Int64 mask)

        [<DllImport("cache_tests.dll", CallingConvention = CallingConvention.Cdecl)>]
        extern void Read_Chunk_x8_DoubleLoop(UInt64 Count, byte* Data, Int64 chunk)

open Native

#nowarn "9"

module Buffers =
    [<Struct>]
    type Buffer =
        { mutable Count: UInt64
          mutable Data: nativeptr<byte> }

    let isInBounds (source: Buffer, at: UInt64) = at < source.Count

    let areEqual (a: Buffer) (b: Buffer) =
        if a.Count <> b.Count then
            false
        else
            let mutable isEqual = true
            let mutable index = 0
            let count = int a.Count

            while index < count do
                if NativePtr.get a.Data index <> NativePtr.get b.Data index then
                    isEqual <- false
                    index <- count // early break

            isEqual

    let allocateBuffer (count: UInt64) : Buffer =
        let size = unativeint count

        { Count = count
          Data = NativeMemory.Alloc size |> NativePtr.ofVoidPtr }

    let freeBuffer (buffer: Buffer byref) =
        if buffer.Data <> Unchecked.defaultof<nativeptr<byte>> then
            NativeMemory.Free(NativePtr.toVoidPtr buffer.Data)

        buffer <- Unchecked.defaultof<Buffer>

    let allocateBufferV totalSize =
        let mem =
            VirtualAlloc(
                IntPtr.Zero,
                uint64 totalSize,
                AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT,
                MemoryProtection.PAGE_READWRITE
            )

        { Count = totalSize
          Data = NativePtr.ofNativeInt mem }

    let freeBufferV (buffer: Buffer byref) =
        if buffer.Data <> NativePtr.nullPtr then
            VirtualFree(NativePtr.toNativeInt buffer.Data, 0UL, FreeType.MEM_RELEASE)
            |> ignore

        buffer <- Unchecked.defaultof<Buffer>

open Buffers

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
            |> Seq.map (fun s -> s.Length)
            |> Seq.reduce (+)
            |> fun count -> int64 count * 1L<b>

    let writeToAllBytes (capacity: uint64) =
        fun () ->
            let mem = Array.create (int capacity) 1L
            int64 mem.Length * 1L<b>

    let writeToAllBytesBuffer (buffer: Buffer) =
        fun () ->
            let count = int buffer.Count - 1

            for i in 0..count do
                NativePtr.set buffer.Data i (byte i)

            int64 buffer.Count * 1L<b>

    let MovAllBytesAsm (buffer: Buffer) =
        fun () ->
            MOVAllBytesASM(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOPAllBytesASM (buffer: Buffer) =
        fun () ->
            NOPAllBytesASM(buffer.Count)
            int64 buffer.Count * 1L<b>

    let CMPAllBytesASM (buffer: Buffer) =
        fun () ->
            CMPAllBytesASM(buffer.Count)
            int64 buffer.Count * 1L<b>

    let DECAllBytesASM (buffer: Buffer) =
        fun () ->
            DECAllBytesASM(buffer.Count)
            int64 buffer.Count * 1L<b>

    let NOP3x1AllBytes (buffer: Buffer) =
        fun () ->
            NOP3x1AllBytes(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOP1x3AllBytes (buffer: Buffer) =
        fun () ->
            NOP1x3AllBytes(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOP1x9AllBytes (buffer: Buffer) =
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

    let getMaxOSRandomCount () : UInt64 = 0xffffffffUL

    let readOSRandomBytes (count: UInt64) (dest: nativeptr<byte>) =
        let mutable result = false

        if count < getMaxOSRandomCount () then
            let r = BCryptGenRandom(0, dest, uint count, 0x00000002u)
            result <- r = 0

        result

    let fillWithRandomBytes (dest: Buffer) =
        let maxRandCount = getMaxOSRandomCount ()
        let mutable atOffset = 0UL

        while atOffset < dest.Count do
            let mutable readCount = dest.Count - atOffset

            if readCount > maxRandCount then
                readCount <- maxRandCount

            let ptr = NativePtr.toNativeInt dest.Data + nativeint atOffset

            if not (readOSRandomBytes readCount (NativePtr.ofNativeInt ptr)) then
                failwithf "fail filling buffer with system random bytes"

            atOffset <- atOffset + readCount

    let fillBufferWithPattern (pattern: BranchPattern) (buffer: Buffer) =
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

    let conditionalNop (count: UInt64) (data: nativeptr<byte>) =
        fun () ->
            ConditionalNOP(count, data)
            int64 count * 1L<b>

    let runReadWriteTests () =
        let gigabyte = 1024UL * 1024UL * 1024UL
        let mutable buffer = allocateBufferV gigabyte

        let functions =
            [| "writeToAllBytesBuffer", writeToAllBytesBuffer buffer
               "NOP3x1AllBytes", NOP3x1AllBytes buffer
               "NOP1x3AllBytes", NOP1x3AllBytes buffer
               "NOP1x9AllBytes", NOP1x9AllBytes buffer |]

        Console.CursorVisible <- false

        while true do
            for name, fn in functions do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"

        freeBufferV &buffer

    let NOPAligned64 (buffer: Buffer) =
        fun () ->
            NOPAligned64(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOPAligned1 (buffer: Buffer) =
        fun () ->
            NOPAligned1(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOPAligned15 (buffer: Buffer) =
        fun () ->
            NOPAligned15(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOPAligned31 (buffer: Buffer) =
        fun () ->
            NOPAligned63(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let NOPAligned63 (buffer: Buffer) =
        fun () ->
            NOPAligned63(buffer.Count, buffer.Data)
            int64 buffer.Count * 1L<b>

    let runJumpAlignments () =
        let gigabyte = 1024UL * 1024UL * 1024UL
        let mutable buffer = allocateBufferV gigabyte

        let functions =
            [| "NOPAligned64", NOPAligned64 buffer
               "NOPAligned1", NOPAligned1 buffer
               "NOP1x3AllBytes", NOPAligned15 buffer
               "NOP1x9AllBytes", NOPAligned31 buffer
               "NOPAligned63", NOPAligned63 buffer |]

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
        let gigabyte = 1024UL * 1024UL * 1024UL
        let mutable buffer = allocateBufferV gigabyte

        let cases =
            [| OSRandom; NeverTaken; AlwaysTaken; Every2; Every3; Every4; RTRandom |]

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

    let executeRatTests () =
        let loopCount = 1000_000_000L

        let ratAdd =
            fun () ->
                RATAdd()
                loopCount * 1L<b>

        let ratMovAdd =
            fun () ->
                RATMovAdd()
                loopCount * 1L<b>

        Console.CursorVisible <- false

        while true do
            for name, fn in [| ("RATAdd", ratAdd); ("RATMovAdd", ratMovAdd) |] do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"

    let runUnrollReadTests () =
        let count = 1024UL * 1024UL * 1024UL

        let readX1 (buffer: Buffer) =
            fun () ->
                Read_x1(count, buffer.Data)
                int64 count * 1L<b>

        let readX2 (buffer: Buffer) =
            fun () ->
                Read_x2(count, buffer.Data)
                int64 count * 1L<b>

        let readX3 (buffer: Buffer) =
            fun () ->
                Read_x3(count, buffer.Data)
                int64 count * 1L<b>

        let readX4 (buffer: Buffer) =
            fun () ->
                Read_x4(count, buffer.Data)
                int64 count * 1L<b>

        let mutable buffer = allocateBufferV 4096UL

        let functions =
            [| "read_x1", readX1 buffer
               "read_x2", readX2 buffer
               "read_x3", readX3 buffer
               "read_x4", readX4 buffer |]

        Console.CursorVisible <- false

        while true do
            for name, fn in functions do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"

        freeBufferV &buffer

    let runByteReadTests () =
        let count = 1024UL * 1024UL * 1024UL

        let read1x2 (buffer: Buffer) =
            fun () ->
                Read_1x2(count, buffer.Data)
                int64 count * 1L<b>

        let read8x2 (buffer: Buffer) =
            fun () ->
                Read_8x2(count, buffer.Data)
                int64 count * 1L<b>


        let mutable buffer = allocateBufferV 4096UL
        let functions = [| "read1x2", read1x2 buffer; "read8x2", read8x2 buffer |]

        Console.CursorVisible <- false

        while true do
            for name, fn in functions do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"

        freeBufferV &buffer

    let runReadWidthsTests () =
        let size = 1024UL * 1024UL * 1024UL

        let read4x2 (buffer: Buffer) =
            fun () ->
                SIMD.Read_4x2(buffer.Count, buffer.Data)
                int64 buffer.Count * 1L<b>

        let read8x2 (buffer: Buffer) =
            fun () ->
                SIMD.Read_8x2(buffer.Count, buffer.Data)
                int64 buffer.Count * 1L<b>

        let read16x2 (buffer: Buffer) =
            fun () ->
                SIMD.Read_16x2(buffer.Count, buffer.Data)
                int64 buffer.Count * 1L<b>

        let read32x2 (buffer: Buffer) =
            fun () ->
                SIMD.Read_32x2(buffer.Count, buffer.Data)
                int64 buffer.Count * 1L<b>

        let mutable buffer = allocateBufferV size

        let functions =
            [| "read4x2", read4x2 buffer
               "read8x2", read8x2 buffer
               "read16x2", read16x2 buffer
               "read32x2", read32x2 buffer |]

        Console.CursorVisible <- false

        while true do
            for name, fn in functions do
                printfn $"--- {name} ---"
                let results = Repetition.repeat true 10L<s> fn
                Repetition.print results
                printfn "\n"

        freeBufferV &buffer

    let showChart dataPoints =
        let chartLayout = Layout.init (Width = 1200, Height = 900)

        Chart.Line(xy = dataPoints, ShowMarkers = true)
        |> Chart.withLayout chartLayout
        |> Chart.withLineStyle (Width = 2., Dash = StyleParam.DrawingStyle.Dot)
        |> Chart.withTitle ("Cache throughput")
        |> Chart.withXAxisStyle ("KB")
        |> Chart.withYAxisStyle ("GB/s")
        |> Chart.show

    let runPowerOfTwoCacheTests () =

        let size = 1024UL * 1024UL * 1024UL

        let readChunk_x4 (buffer: Buffer) mask =
            fun () ->
                SIMD.Read_Chunk_x4(buffer.Count, buffer.Data, mask)
                int64 buffer.Count * 1L<b>

        let readChunk_x8 (buffer: Buffer) mask =
            fun () ->
                SIMD.Read_Chunk_x8(buffer.Count, buffer.Data, mask)
                int64 buffer.Count * 1L<b>

        let mutable buffer = allocateBufferV size

        printfn "touching memory"

        for i in 0UL .. buffer.Count do
            NativePtr.set buffer.Data (int i) (byte i)

        let functions =
            [| "readChunk_x4", readChunk_x4 buffer; "readChunk_x8", readChunk_x8 buffer |]

        Console.CursorVisible <- false

        let allResults = ResizeArray()

        for name, fn in functions do
            for mask in
                [| 4095L
                   8191L
                   16383L
                   32767L
                   65535L
                   131071L
                   262143L
                   524287L
                   1048575L
                   2097151L
                   3145727L
                   4194303L
                   5242879L
                   6291455L
                   8388607L
                   10485759L |] do
                printfn $"--- {name}: {(mask + 1L) / 1024L}Kb 0x{Convert.ToString(mask, 16)} ---"
                let results = Repetition.repeat true 10L<s> (fn mask)
                Repetition.print results
                allResults.Add((mask + 1L, results))

        let dataPoints =
            [| for (size, result) in allResults do
                   let count = result.Count
                   let cpuFreq = result.CPUFreq
                   let result = result.Min
                   let seconds = Timing.secondsFromCpuTime cpuFreq (result.CPUTime / count)
                   let bandwidth = Timing.bandwidth (result.BytesCount / count) seconds

                   size, bandwidth |]

        showChart dataPoints

        freeBufferV &buffer

    let runCacheTests () =
        let size = 1024UL * 1024UL * 1024UL

        let readChunk_x8DoubleLoop (buffer: Buffer) chunk =
            fun () ->
                let count = (buffer.Count / uint64 chunk) * uint64 chunk
                SIMD.Read_Chunk_x8_DoubleLoop(count, buffer.Data, chunk)
                int64 count * 1L<b>

        let mutable buffer = allocateBufferV size

        printfn "touching memory"

        for i in 0UL .. buffer.Count do
            NativePtr.set buffer.Data (int i) (byte i)

        let functions = [| "readChunk_x8_DoubleLoop", readChunk_x8DoubleLoop buffer |]

        Console.CursorVisible <- false

        let allResults = ResizeArray()

        for name, fn in functions do
            for chunk in
                [| 30720L // 30K
                   32000L // around 31K
                   32256L // around 32K
                   32512L
                   32768L // 32K
                   33024L
                   33280L // around 32K
                   523520L // around 512 KB
                   523776L
                   524032L
                   524288L // 512K
                   524544L
                   524800L // around 512K
                   1048576L
                   2097152L
                   3145728L
                   4193280L // around 4MB
                   4193536L
                   4193792L
                   4194048L
                   4194304L // 4MB
                   4194560L
                   4194816L
                   4195072L
                   4195328L // around 4MB
                   5241856L // around 5 MB
                   5242112L
                   5242368L
                   5242624L
                   5242880L // 5MB
                   5243136L
                   5243392L
                   5243648L
                   5243904L // around 5MB
                   6290432L // around 6MB
                   6290688L
                   6290944L
                   6291200L
                   6291456L // 6MB
                   6291712L
                   6291968L
                   6292224L
                   6292480L // around 6MB
                   7340032L // 7MB
                   8387584L //around 8MB
                   8387840L
                   8388096L
                   8388352L
                   8388608L // 8MB
                   8388864L
                   8389120L
                   8389376L
                   8389632L // around 8MB
                   9437184L // 9 MB
                   10485760L // 10MB
                   12582912L // 12 MB
                   15728640L // 15 MB
                   // 20971520L // 20 MB
                   // 52428800L // 50 MB
                   // 83886080L // 80 MB
                   // 104857600L // 100MB
                   // 1073741824L // 1GB
                   |] do
                printfn $"--- {name}: {(chunk + 1L) / 1024L}Kb 0x{Convert.ToString(chunk, 16)} ---"
                let results = Repetition.repeat true 10L<s> (fn chunk)
                Repetition.print results
                printfn "\n"
                allResults.Add((chunk + 1L, results))

        let dataPoints =
            [| for (size, result) in allResults do
                   let count = result.Count
                   let cpuFreq = result.CPUFreq
                   let result = result.Min
                   let seconds = Timing.secondsFromCpuTime cpuFreq (result.CPUTime / count)
                   let bandwidth = Timing.bandwidth (result.BytesCount / count) seconds

                   size, bandwidth |]

        showChart dataPoints
        freeBufferV &buffer
