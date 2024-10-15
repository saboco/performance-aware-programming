module SystemTesting.SetAssociativity
open System.IO
open Memory
open Buffers
open Microsoft.FSharp.NativeInterop
open Diagnostics

#nowarn "9"

module Native =
        
    open System
    open System.Runtime.InteropServices
    
    [<DllImport("read_stride.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void ReadStrided_32x2(UInt64 Count, byte* Data, Int64 ReadPerBlock, Int64 Stride)

let runSetAssociativity () =
    let cacheLineSize = 64
    let repCount = 64
    let readCount = 256
    let totalTests = 128
    let totalBytes = repCount * readCount * cacheLineSize
    
    let readStrided (buffer: Buffer) stride totalBytes =
        fun () ->
            Native.ReadStrided_32x2(uint64 repCount, buffer.Data, readCount, stride)
            int64 totalBytes  * 1L<b>
    
    let size = readCount * cacheLineSize * totalTests
    let mutable buffer = Buffers.allocateBufferV (uint64 size)
    
    printfn "touching memory"

    for i in 0UL .. buffer.Count - 1UL do
        NativePtr.set buffer.Data (int i) (byte i)
    
    let allResults = ResizeArray()
    for strideIndex in 0 .. totalTests - 1 do
        let stride = cacheLineSize * strideIndex
        printf $"\n--- ReadStrided_32x2 of {readCount} lines spaced {stride} bytes apart (total span: {readCount * stride}) ---\n"
        
        let results = Repetition.repeat true 10L<s> (readStrided buffer stride totalBytes) 
        Repetition.print results
        printfn "\n"
        allResults.Add((stride, results))
        
    let dataPoints =
        [| for (size, result) in allResults do
               let cpuFreq = result.CPUFreq
               let result = result.Min
               let seconds = Timing.secondsFromCpuTime cpuFreq result.CPUTime
               let bandwidth = Timing.bandwidth result.BytesCount seconds

               size, bandwidth |]

    Print.showChart dataPoints
    
    dataPoints
    |> Array.map (fun (s, bw) -> printfn $"{s},{bw}"; $"{s},{bw}")
    |> fun lines -> File.AppendAllLines(@"c:\temp\cache_set_associativity_data.txt", lines)