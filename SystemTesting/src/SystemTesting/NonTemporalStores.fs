module SystemTesting.NonTemporalStores

open System
open Microsoft.FSharp.NativeInterop
open SystemTesting.Diagnostics
open Memory
open Buffers

module Native =
    open System.Runtime.InteropServices
    
    [<DllImport("non_temporal_stores.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void StandardWrite(UInt64 Count, byte* Data, UInt64 innerLoopCount, byte* table)
    
    [<DllImport("non_temporal_stores.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void StreamingWrite(UInt64 Count, byte* Data, UInt64 innerLoopCount, byte* table)

let runNonTemporalStores () =
    
    let standardWrite (src: Buffer) (dst: Buffer) outerLoopCount innerLoopCount =
        fun () ->            
            Native.StandardWrite(outerLoopCount, src.Data, innerLoopCount, dst.Data)
            int64 dst.Count * 1L<b>
            
    let streamingWrite (src: Buffer) (dst: Buffer) outerLoopCount innerLoopCount =
        fun () ->            
            Native.StreamingWrite(outerLoopCount, src.Data, innerLoopCount, dst.Data)
            int64 dst.Count * 1L<b>
        
    let mutable src = allocateBufferV (1024UL * 1024UL)
    let mutable dst = allocateBufferV (1024UL * 1024UL * 1024UL)
    let innerLoopSize = 256UL

    let functions =
        [| "standardWrite", standardWrite src dst
           "streamingWrite", streamingWrite src dst |]
        
    printfn "touching memory"
    
    for i in 0UL .. src.Count - 1UL do
        NativePtr.set src.Data (int i) (byte i)

    Console.CursorVisible <- false
    let rec loop sourceCount =
        if sourceCount <= src.Count then
            let innerLoopCount = sourceCount / innerLoopSize
            let outerLoopCount = dst.Count / (innerLoopSize * innerLoopCount)
            
            for name, f in functions do
                printfn $"\n--- {name} of {sourceCount} bytes ---\n"
                
                for i in 0UL .. dst.Count - 1UL do
                    NativePtr.set dst.Data (int i) 0uy
                
                let results = Repetition.repeat true 10L<s> (f outerLoopCount innerLoopCount)
                Repetition.print results
                
                for i in 0UL .. dst.Count - 1UL do
                    let srcIndex = int (i % sourceCount)
                    if (NativePtr.get dst.Data (int i)) <> (NativePtr.get src.Data srcIndex) then
                        printfn $"Error: destination written incorrectly"
                    
                printfn "\n"
                
            loop (sourceCount * 2UL)
        
    loop innerLoopSize
        
    freeBufferV &src
    freeBufferV &dst