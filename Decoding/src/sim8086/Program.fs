module sim8086.Program

open System.IO
open Decoder

let testDecoding () =
    let srcDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding\input/decoding"
    let dstDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding/output/"
    let files = Directory.EnumerateFiles(srcDirectory,"*", SearchOption.TopDirectoryOnly)
    for file in files do
        let fileName = Path.GetFileName(file)
        let mutable at = 0
        if Path.GetExtension(file) = "" then
            let dst = $"decoded_{fileName}.asm" 
            let stream = File.ReadAllBytes file
            let instructions = decodeAll stream
            let asm = Registers.printInstructions instructions
            File.WriteAllText(Path.Combine(dstDirectory, dst), asm)

let testComputer () =
    let srcDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding\input/computing"
    let files = Directory.EnumerateFiles(srcDirectory,"*", SearchOption.TopDirectoryOnly)
    for file in files do
        let fileName = Path.GetFileName(file)
        let mutable at = 0
        printfn $"%s{fileName}"
        if Path.GetExtension(file) = "" && fileName.Contains("57") then
            let stream = File.ReadAllBytes file
            Computer.registers <- Array.zeroCreate Computer.registers.Length
            Computer.flags <- [|0us|]
            Computer.memory <- Array.zeroCreate Computer.memory.Length
            let initialRegisters = Array.copy Computer.registers
            let initialFlags = Computer.flags[0]
            let timer = Computer.Timing.Timer()
            while at < stream.Length do
                for i in Computer.executeProgram timer stream at do
                    at <- i
                    
            printfn ""
            printfn "Final registers:"
            Computer.printRegistersN initialRegisters Computer.registers
            printfn ""            
            Computer.Flags.printFlags initialFlags Computer.flags[0]
            printfn ""
            let (operations, cycles, cyclesPerOperation) = timer.GetCounters()
            printfn $"cycles={cycles}\toperations={operations}\t{cyclesPerOperation}"

[<EntryPoint>]
let main (argv : string[]) =
    testComputer ()
    0
    
// TODO: load program to memory
