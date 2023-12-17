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


System.IO.File.ReadAllBytes("./homewok/listing_0040_challenge_movs")
|> PrintTools.printBinary