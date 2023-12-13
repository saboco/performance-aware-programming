open System.Text
open System.IO


module printTools = 
    let printByte (byte : byte) = printf $"%B{byte}"
    let printBinary (bytes: byte[]) =
        let mutable first = true
        for byte in bytes do
            if not first then 
                printf " "
            else
                first <- false
            
            printByte byte
        
        printfn ""
    

module Decoding8086 =
    let mov = 0b10001000uy
    let getFirst6Bits byte = (byte >>> 2) &&& 0b00111111uy
    let get7thBit byte = (byte >>> 1) &&& 0b00000001uy
    let get8thBit byte = byte &&& 0b00000001uy
    let getFirst2Bits byte = (byte >>> 6) &&& 0b00000011uy
    let get3thto5thBits byte = (byte >>> 3) &&&  0b00000111uy
    let get6thto8thBits byte = byte &&& 0b00000111uy
    let getOpCode byte = getFirst6Bits byte
    let getDirection byte = get7thBit byte
    let getWord byte = get8thBit byte
    let getMod byte = getFirst2Bits byte
    let getREG byte = get3thto5thBits byte
    let getRM byte = get6thto8thBits byte

    // instuction 
    // opcode destination, source

    // encoded instruction 2 bytes
    // ### Byte 1 ###
    // first 6 bits: opcode
    // 7th bit, direction:
    //      * 1 destination addr is in REG (to register)
    //      * 0 source addr is in REG (from register)
    // 8th bit, word/byte operation:
    //      * 0: byte operation (byte= 8 bits)
    //      * 1: word operation (a word in a 16 bit processor is 16 bits wide)
    //
    // ### Byte 2 ###
    // first 2 bits: mod
    // 3th to 5th bit (3 bits): REG identifier
    // 6th to 8th bit (3 bits): RM (register operand/register to use in ea calculation)

    let [<Literal>] REG_IS_SOURCE = 0b0uy
    let [<Literal>] REG_IS_DESTINATION = 0b1uy
    let [<Literal>] MEMORY_MODE_00_DISPLACEMENT = 0b00uy
    let [<Literal>] MEMORY_MODE_08_DISPLACEMENT = 0b01uy
    let [<Literal>] MEMORY_MODE_16_DISPLACEMENT = 0b10uy
    let [<Literal>] REGISTER_TO_REGISTER_MODE = 0b11uy

    let registers w id =
        match id with
        | 0b000uy -> if w then "ax" else "al"
        | 0b001uy -> if w then "cx" else "cl"
        | 0b010uy -> if w then "dx" else "dl"
        | 0b011uy -> if w then "bx" else "bl"
        | 0b100uy -> if w then "sp" else "ah"
        | 0b101uy -> if w then "bp" else "ch"
        | 0b110uy -> if w then "si" else "dh"
        | 0b111uy -> if w then "di" else "bh"
        | b -> failwithf $"Unkown register: %B{b}"

    let opcodes opcode =
        match opcode with
        | MOV -> "mov"
        | opcode -> failwithf $"opcode '%B{opcode}' not yet supported"

    let decode (bytes : byte[]) =
        let sb = StringBuilder()
        sb.AppendLine("bits 16\n")
        for i in 0..2..bytes.Length-1 do

            let byte0 = bytes[i]
            let opcode = getOpCode byte0
            let d = getDirection byte0
            let w = not (getWord byte0 = 0b0uy)

            let byte1 = bytes[i+1]
            let ``mod`` = getMod byte1
            
            let reg =  getREG byte1
            let rm = getRM byte1

            let (source, destination) =
                match ``mod`` with
                | MEMORY_MODE_00_DISPLACEMENT -> failwithf "not yet supportted"
                | MEMORY_MODE_08_DISPLACEMENT -> failwithf "not yet supportted"
                | MEMORY_MODE_16_DISPLACEMENT -> failwithf "not yet supportted"
                | REGISTER_TO_REGISTER_MODE ->
                    let source =
                        match d with
                        | REG_IS_SOURCE -> registers w reg
                        | REG_IS_DESTINATION -> registers w rm

                    let destination = 
                        match d with
                        | REG_IS_DESTINATION -> registers w reg
                        | REG_IS_SOURCE ->  registers w rm
                    
                    source, destination
                | ``mod`` -> failwithf $"'%B{``mod``}' mode is invalid"
        
            let operation = opcodes opcode
                
            sb.AppendLine($"{operation} {destination}, {source}")

        sb.ToString()

    let decodeFile source destination = 
        File.ReadAllBytes(source)
        |> decode 
        |> fun text -> File.WriteAllText(destination, text)

Decoding8086.decodeFile "./listing_0037_single_register_mov" "./decoded_single_register_move.asm"
Decoding8086.decodeFile "./listing_0038_many_register_mov" "./decoded_many_register_move.asm"