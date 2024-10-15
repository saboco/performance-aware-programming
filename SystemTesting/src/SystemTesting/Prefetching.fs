module SystemTesting.Prefetching

open System
open System.IO
open Microsoft.FSharp.NativeInterop
open Memory
open Buffers
open Diagnostics
open SystemTesting.Diagnostics.Repetition

module Native =
    open System.Runtime.InteropServices
    
    [<DllImport("prefetching.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void PeriodicRead(UInt64 Count, byte* Data, UInt64 innerLoopCount)
    
    [<DllImport("prefetching.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void PeriodicPrefetchedRead(UInt64 Count, byte* Data, UInt64 innerLoopCount)
    
let initBufferToRandomJumpPattern (buffer : Buffer) outerLoopCount cacheLineSize cacheLineCount =
    let mutable jumpOffset = 0UL
    
    for outerLoopIndex in 0UL .. outerLoopCount - 1UL do
       let mutable nextOffset = 0UL
       let mutable nextPointer = NativePtr.nullPtr<UInt64>
       
       let mutable randomValue = 0UL
       let ptrRandomValue = NativePtr.toNativeInt &&randomValue |> NativePtr.ofNativeInt<byte>
      
       readOSRandomBytes (uint64 sizeof<UInt64>) ptrRandomValue |> ignore
       
       let rec searchLoop searchIndex =
          if searchIndex < cacheLineCount then
              nextOffset <- (randomValue + searchIndex) % cacheLineCount
              nextPointer <- NativePtr.toNativeInt buffer.Data + nativeint (nextOffset * cacheLineSize) |> NativePtr.ofNativeInt
              
              if NativePtr.read nextPointer = 0UL then
                  true
              else
                  searchLoop (searchIndex + 1UL)
          else
              false
           
       if not (searchLoop 0UL) then
           printfn "unable to create a single continuous pointer chain"
           
       let jumpData =
           (NativePtr.toNativeInt buffer.Data) + (nativeint (jumpOffset * cacheLineSize))
           |> NativePtr.ofNativeInt
           
       NativePtr.write jumpData (uint64 (NativePtr.toNativeInt nextPointer))
       NativePtr.write (NativePtr.add jumpData 1) outerLoopIndex
       jumpOffset <- nextOffset
     
let runPrefetching () =
    printfn "Prefetching"
    
    let mutable buffer = allocateBufferV (1024UL * 1024UL * 1024UL)
    
    let outerLoopCount = 1024UL * 1024UL
    let cacheLineSize = 64UL
    let testSize = cacheLineSize * outerLoopCount
    let cacheLineCount = buffer.Count / cacheLineSize
   
    let periodicRead (buffer : Buffer) outerLoopCount innerLoopCount =
        fun () ->
            Native.PeriodicRead(outerLoopCount, buffer.Data, uint64 innerLoopCount)
            int64 testSize * 1L<b>
            
    let periodicPrefetchedRead (buffer : Buffer) outerLoopCount innerLoopCount =
        fun () ->
            Native.PeriodicPrefetchedRead (outerLoopCount, buffer.Data, uint64 innerLoopCount)
            int64 testSize * 1L<b>
            
    let functions = [|
        "periodicRead", periodicRead buffer
        "periodicPrefetchedRead", periodicPrefetchedRead buffer
    |]
    
    initBufferToRandomJumpPattern buffer outerLoopCount cacheLineSize cacheLineCount
       
    let testsResults = Array.create<int * RepetitionsResult[]> 32 Unchecked.defaultof<int * RepetitionsResult[]> 
    for innerLoopIndex in 0 .. 31 do
        let mutable innerLoopCount = 4 * (innerLoopIndex + 1)
        if innerLoopIndex >= 16 then
            innerLoopCount <- 64 * (innerLoopIndex - 14)
        
        let result = Array.create<RepetitionsResult> 2 Unchecked.defaultof<RepetitionsResult>
        
        let mutable functionIndex = 0
        for name, f in functions do
            printfn $"\n\n--- {name} ({innerLoopCount} inner loop iterations)---\n"
            result[functionIndex] <-  Repetition.repeat true 10L<s> (f outerLoopCount innerLoopCount)
            functionIndex <- functionIndex + 1           
        
        testsResults[innerLoopIndex] <- innerLoopCount, result
    
    let dataPoints = [| 
        for functionIndex in 0..1 do
            let points =
                [|for innerLoopIndex in 0 .. 31 do
                    let innerLoopCount, results = testsResults[innerLoopIndex]
                 
                    let result = results[functionIndex]
                    let cpuFreq = result.CPUFreq
                    let result = result.Min
                    let seconds = Timing.secondsFromCpuTime cpuFreq result.CPUTime
                    let bandwidth = Timing.bandwidth result.BytesCount seconds
                    
                    innerLoopCount, bandwidth |]
                |> Seq.ofArray
            let name, _ = functions.[functionIndex]
            name, points
    |]
    
    Print.showMultiLineChart "Prefetching" dataPoints
    
    let dataPoints = [| 
        for innerLoopIndex in 0 .. 31 do
            let innerLoopCount, results = testsResults[innerLoopIndex]
             
            let bandwidths = [|       
                for functionIndex in 0..1 do
                let result = results[functionIndex]
                let cpuFreq = result.CPUFreq
                let result = result.Min
                let seconds = Timing.secondsFromCpuTime cpuFreq result.CPUTime
                let bandwidth = Timing.bandwidth result.BytesCount seconds
                
                bandwidth |]
            innerLoopCount, bandwidths[0], bandwidths[1]
    |]
    
    dataPoints
    |> Array.map (fun (count, bw0, bw1) -> printfn $"{count},{bw0},{bw1}"; $"{count},{bw0},{bw1}")
    |> fun lines -> File.AppendAllLines(@"c:\temp\cache_prefetching.txt", lines)
    
    freeBufferV &buffer