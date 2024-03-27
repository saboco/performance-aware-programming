module SystemTesting.PageFault

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Diagnostics
open System.IO.MemoryMappedFiles
open System.IO
open Memory
open Native

let touchByInt64 _ touchSize (dPtr: nativeint) =
    let mutable dst: nativeptr<Int64> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
    let mutable index = 0L
    let size = int64 sizeof<Int64>

    while index < touchSize do
        index |> NativePtr.write dst
        dst <- NativePtr.add dst 1
        index <- index + size

let touchByteByByte _ touchSize (dPtr: nativeint) =
    let mutable dst: nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
    let mutable index = 0L

    while index < touchSize do
        (byte index) |> NativePtr.write dst
        dst <- NativePtr.toNativeInt dst + 1n |> NativePtr.ofNativeInt
        index <- index + 1L

let touchByInt64Backwards (totalSize: int64) touchSize (dPtr: nativeint) =
    let mutable dst: nativeptr<Int64> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
    let mutable index = 0L
    let size = int64 sizeof<Int64>
    dst <- NativePtr.add dst (int (totalSize / size))
    dst <- NativePtr.add dst -1

    while index < touchSize do
        index |> NativePtr.write dst
        dst <- NativePtr.add dst -1
        index <- index + size

let touchByteByByteBackwards totalSize touchSize (dPtr: nativeint) =
    let mutable dst: nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr
    let mutable index = 0L
    dst <- NativePtr.add dst (int totalSize - 1)

    while index < touchSize do
        (byte index) |> NativePtr.write dst
        dst <- NativePtr.add dst -1
        index <- index + 1L

let pageAllocationTest
    (touchMemory: int64 -> int64 -> nativeint -> unit)
    (memAlloc: int -> nativeint)
    (memFree: nativeint -> unit)
    pageCount
    =
    Native.InitializeMetrics()
    let pageSize = 4096L
    let totalSize = pageCount * pageSize
    let results = ResizeArray<string>(int pageCount)

    for touchCount in 1L .. pageCount do
        let touchSize = pageSize * touchCount
        let dPtr = memAlloc (int totalSize)
        let startFaultCount = Native.ReadPageFaultCount()
        touchMemory totalSize touchSize dPtr
        let endFaultCount = Native.ReadPageFaultCount()
        let faultCount = endFaultCount - startFaultCount
        memFree dPtr
        let addr = VirtualAddress.decomposePointer4K (dPtr + nativeint touchSize - 1n)

        results.Add(
            $"{pageCount};{touchCount};{faultCount};{int64 faultCount - touchCount};{Option.defaultValue 0us addr.DirectoryIndex};{Option.defaultValue 0us addr.TableIndex}"
        )

    results

let pageAllocationAnomalyTest printPointer memAlloc memFree (pageCount: uint64) =
    Native.InitializeMetrics()
    let pageSize = 4096UL
    let totalSize = pageCount * pageSize
    let results = ResizeArray<string>(int pageCount)
    let mutable priorOverFaultCount = 0UL
    let mutable priorPageIndex = 0UL

    let dPtr: nativeint = memAlloc (int totalSize)
    let mutable dst: nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr

    let startFaultCount = Native.ReadPageFaultCount()
    let mutable pageIndex = 0UL
    let pageSize = 4096UL
    dst <- NativePtr.add dst (int totalSize - 1) // last byte of the memory

    while pageIndex < pageCount do
        (byte pageIndex) |> NativePtr.write dst
        let endFaultCount = Native.ReadPageFaultCount()
        let overFaultCount = (endFaultCount - startFaultCount) - pageIndex

        if overFaultCount > priorOverFaultCount then
            printfn
                $"Page %i{pageIndex}: %i{overFaultCount} extra faults (%i{pageIndex - priorPageIndex} pages since last increase)"

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

let pageAllocationAnomalyTest2 printPointer memAlloc memFree (pageCount: uint64) =
    Native.InitializeMetrics()
    let pageSize = 4096UL
    let totalSize = pageCount * pageSize
    let results = ResizeArray<string>(int pageCount)
    let mutable priorOverFaultCount = 0UL
    let mutable priorPageIndex = 0UL

    let dPtr: nativeint = memAlloc (int totalSize)
    let mutable dst: nativeptr<byte> = dPtr.ToPointer() |> NativePtr.ofVoidPtr

    let startFaultCount = Native.ReadPageFaultCount()
    let mutable pageIndex = 0UL
    let pageSize = 4096UL
    dst <- NativePtr.add dst (int totalSize - 1) // last byte of the memory

    while pageIndex < pageCount do
        (byte pageIndex) |> NativePtr.write dst
        let endFaultCount = Native.ReadPageFaultCount()
        let overFaultCount = (endFaultCount - startFaultCount) - pageIndex

        if overFaultCount > priorOverFaultCount then
            printfn
                $"Page %i{pageIndex}: %i{overFaultCount} extra faults (%i{pageIndex - priorPageIndex} pages since last increase)"

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
        MemoryProtection.PAGE_READWRITE
    )

let virtualFree (ptr: nativeint) =
    VirtualFree(ptr, 0UL, FreeType.MEM_RELEASE) |> ignore

let pageAllocationVirtualAlloc touchMemory pageCount =
    pageAllocationTest touchMemory virtualAlloc virtualFree pageCount

let pageAllocWithMemoryMappedFile pageCount =
    Native.InitializeMetrics()
    let pageSize = 4096L
    let totalSize = pageCount * pageSize
    let results = ResizeArray<string>(int pageCount)

    for touchCount in 1L .. pageCount do
        let touchSize = pageSize * touchCount

        let mMap =
            MemoryMappedFile.CreateNew(
                $"page_alloc_test_{touchCount}",
                int64 totalSize,
                MemoryMappedFileAccess.ReadWrite
            )

        let mMapA = mMap.CreateViewAccessor()
        let mutable dst: nativeptr<byte> = NativePtr.nullPtr<byte>
        mMapA.SafeMemoryMappedViewHandle.AcquirePointer(&dst)
        let mutable index = 0L
        let mutable currentByteIndex = 0
        let size = sizeof<Int64>
        let startFaultCount = Native.ReadPageFaultCount()

        while index < touchSize do
            let mutable src: nativeptr<byte> =
                &&index |> NativePtr.toNativeInt |> NativePtr.ofNativeInt

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

let executePageFaultProbe (pageCount: UInt64) =
    let results = ResizeArray<string>()

    let putResult (pageCount: UInt64) (touchCount: UInt64) (faultCount: UInt64) (extraFaultCount: UInt64) : unit =
        results.Add($"{pageCount};{touchCount};{faultCount};{extraFaultCount}")

    Native.ProbePageFaults(pageCount, Native.PutResult(putResult))
    results

let run () =
    let stamp () =
        DateTime.Now.ToString("dd_MM_yyyy_HH_mm_ss")

    // printfn "Executing pageAllocationAnomalyTest"
    // let results = pageAllocationAnomalyTest true virtualAlloc virtualFree 16384UL
    // File.AppendAllLines($"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationAnomalyTest_{stamp ()}.csv", results)
    //
    printfn "Executing pageAllocationVirtualAllocByteByByte"
    let results = pageAllocationVirtualAlloc touchByteByByte 4096L

    File.AppendAllLines(
        $"{__SOURCE_DIRECTORY__}\..\..\output\pageAllocationVirtualAllocByteByByte_{stamp ()}.csv",
        results
    )
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
