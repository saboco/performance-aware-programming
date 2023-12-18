open System
open System.IO
open sim8086
open Decoder

let program stream at = seq {
    for instruction, at in Decoder.decode stream at do
        let oldRegisters = Array.copy Computer.registers
        let oldFlags = Computer.flags[0]
        yield! Computer.executeInstruction instruction at
        printf $"{Registers.sprintInstruction instruction};\t"
        Computer.printRegisters oldRegisters Computer.registers
        printf ";\t"
        Computer.Flags.printFlags oldFlags Computer.flags[0]
        printfn ""
}

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
        if Path.GetExtension(file) = "" then
            let stream = File.ReadAllBytes file
            while at < stream.Length do
                for i in program stream at do
                    at <- i

[<EntryPoint>]
let main (argv : string[]) =
    testComputer ()
    0
