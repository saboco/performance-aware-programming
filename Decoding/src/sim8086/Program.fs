open System
open sim8086

[<EntryPoint>]
let main (argv : string[]) =
    let src = if argv.Length > 0 then argv[0] else "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding\homework/listing_0040_challenge_movs"
    let dst = if argv.Length > 1 then argv[1] else "C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Decoding/output/decoded_challenge_movs.asm"
    let asm = Decoder.Decoding8086.decodeFile src dst
    Console.Write(asm)
    
    0
