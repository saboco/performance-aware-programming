open System
open System.IO
open sim8086
open Decoder
open Computer

[<EntryPoint>]
let main (argv : string[]) =
    let srcDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding\input/"
    let dstDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding/output/"
    let files = Directory.EnumerateFiles(srcDirectory)
    
    for file in files do
        if Path.GetExtension(file) = "" && Path.GetFileName(file).Contains("47") then
            let dst = $"decoded_{Path.GetFileName(file)}.asm" 
            let instructions = decodeFile file
            let before = Array.copy Computer.registers
            Computer.executeInstructions instructions
            Computer.printRegisters before Computer.registers
            File.WriteAllText(Path.Combine(dstDirectory, dst), Registers.printInstructions instructions)
            
    0
