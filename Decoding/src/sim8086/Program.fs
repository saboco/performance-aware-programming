open System
open System.IO
open sim8086

[<EntryPoint>]
let main (argv : string[]) =
    let srcDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding\homework/"
    let dstDirectory = "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding/output/"
    let files = Directory.EnumerateFiles(srcDirectory)
    
    for file in files do
        if Path.GetExtension(file) = "" then
            let dst = $"decoded_{Path.GetFileName(file)}.asm" 
            let asm = Decoder.Decoding8086.decodeFile file
            File.WriteAllText(Path.Combine(dstDirectory, dst), asm)
            Console.Write(asm)
    0
