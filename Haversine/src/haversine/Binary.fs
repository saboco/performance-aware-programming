module haversine.Binary
open System

let printBinaryBits (value : UInt64) (firstBit : UInt32) (bitCount : UInt32) =
    for bitIndex in 0u..(bitCount-1u) do
        let offset = int32 ((bitCount - 1u - bitIndex) + firstBit)
        let bit = (value >>> offset) &&& 1UL;
        let bit = if bit > 0UL then '1' else '0'
        printf $"%c{bit}"
        
let printByte (byte : byte) = printf $"%08B{byte}"
let printByteWithPrefix (prefix : string) (byte : byte) =
    printf $"{prefix} "
    printByte byte
    
let printBinary (bytes: byte[]) =
    let mutable first = true
    for i in (bytes.Length-1)..(-1)..0 do
        if not first then 
            printf " "
        else
            first <- false
        
        let byte = bytes[i]
        
        printf $"[%i{i}] "
        printByte byte
    
    printfn ""
    
let printNumberToBinary (n : int) =
    BitConverter.GetBytes(n)
    |>printBinary