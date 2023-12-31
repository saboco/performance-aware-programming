module PrintTools = 
    let printByte (byte : byte) = printf $"%08B{byte}"
    let printByteWithPrefix (prefix : string) (byte : byte) =
        printf $"{prefix} "
        printByte byte
    let printBinary (bytes: byte[]) =
        let mutable first = true
        let mutable i = 0
        for byte in bytes do
            if not first then 
                printf " "
            else
                first <- false
            
            printf $"[%i{i}] "
            printByte byte
            i <- i + 1
        
        printfn ""

System.IO.File.ReadAllBytes("./homework/listing_0040_challenge_movs")
|> PrintTools.printBinary
 
let a = 128
let lo = int8 (int16 a) |> byte
let hi = (int16 a) >>> 8 |> byte

$"%016B{a}\n"
PrintTools.printByte lo
PrintTools.printByte hi

let three = 3
let lo3 = byte (int8 (int16 three))
let hi3 = (int16 three) >>> 8 |> byte

$"0x%04x{290}"

let read16bits (lo : byte) (hi : byte) =
    let bl = uint16 lo
    let bh = uint16 hi
    int ((bh <<< 8) |||  bl)

read16bits lo3 hi3