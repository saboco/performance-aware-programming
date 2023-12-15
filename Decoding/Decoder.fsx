open System.Text
open System.IO

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

module Decoding8086 =
    let get7thBit byte = (byte >>> 1) &&& 0b00000001uy
    let get8thBit byte = byte &&& 0b00000001uy
    let getFirst2Bits byte = (byte >>> 6) &&& 0b00000011uy
    let get3thto5thBits byte = (byte >>> 3) &&&  0b00000111uy
    let getLast3Bits byte = byte &&& 0b00000111uy
    let get5thBit byte = (byte >>> 3) &&& 0b00000001uy
 
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
    
    // opcodes
    let [<Literal>] MOV = 0b100010uy
    let [<Literal>] MOV_IMMEDIATE_TO_REGISTER = 0b1011uy
    let [<Literal>] MOVE_IMMEDIATE_TO_MEMORY_OR_REGISTER = 0b110001uy
    let [<Literal>] MOVE_TO_FROM_ACCUMULATOR = 0b101000uy
    let [<Literal>] ADD = 0b000000uy
    let [<Literal>] ADD_IMMEDIATE_TO_MEMORY_OR_REGISTER = 0b100000uy
    let [<Literal>] ADD_IMMEDIATE_TO_ACCUMULATOR = 0b000001uy
    let [<Literal>] SUB = 0b001010uy
    let [<Literal>] SUB_IMMEDIATE_TO_MEMORY_OR_REGISTER = 0b100000uy
    let [<Literal>] SUB_IMMEDIATE_TO_ACCUMULATOR = 0b001011uy
    let [<Literal>] CMP = 0b001110uy
    let [<Literal>] CMP_IMMEDIATE_TO_MEMORY_OR_REGISTER = 0b100000uy
    let [<Literal>] CMP_IMMEDIATE_TO_ACCUMULATOR = 0b001111uy
    let [<Literal>] JMP_EQUAL_ZERO_OR_NEGATION = 0b011101uy
    let [<Literal>] JPM_ON_LESS_OR_LESS_OR_EQUAL_OR_NEGATION = 0b011111uy
    let [<Literal>] JMP_ON_BELOW_OR_NEGATION = 0b011100uy
    let [<Literal>] JMP_ON_BELOW_OR_EQUAL_OR_NEGATION = 0b011101uy
    let [<Literal>] JMP_ON_PARITY_OR_NEGATION = 0b011110uy
    let [<Literal>] JMP_ON_OVERFLOW_OR_NEGATION = 0b011100uy
    let [<Literal>] JMP_ON_SIGN_OR_NEGATION = 0b011110uy
    let [<Literal>] LOOP = 0b111000uy

    // memory modes
    let [<Literal>] MEMORY_MODE_00_DISPLACEMENT = 0b00uy
    let [<Literal>] MEMORY_MODE_08_DISPLACEMENT = 0b01uy
    let [<Literal>] MEMORY_MODE_16_DISPLACEMENT = 0b10uy
    let [<Literal>] REGISTER_TO_REGISTER_MODE = 0b11uy

    // REG operand
    let [<Literal>] REG_IS_SOURCE = 0b0uy
    let [<Literal>] REG_IS_DESTINATION = 0b1uy

    let read16bits bl bh : uint16 = 
        let bl = uint16 bl
        let bh = uint16 bh
        ((bh <<< 8) ||| 0b11111111us) &&& ((0b11111111us <<< 8) ||| bl)
        //((bh <<< 8) |||  bl)

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
    let rms00 rm (offset : uint16) =
        match rm with
        | 0b000uy -> "[bx + si]"
        | 0b001uy -> "[bx + di]"
        | 0b010uy -> "[bp + si]"
        | 0b011uy -> "[bp + di]"
        | 0b100uy -> "[si]"
        | 0b101uy -> "[di]"
        | 0b110uy -> $"[%i{offset}]"
        | 0b111uy -> "[bx]"
        | b -> failwithf $"Unkown rm: %B{b}"
    let rmsDisplaced rm (offset : uint16) = 
        match rm with
        | 0b000uy -> if offset = 0us then $"[bx + si]" else $"[bx + si + %i{offset}]"
        | 0b001uy -> if offset = 0us then $"[bx + di]" else $"[bx + di + %i{offset}]"
        | 0b010uy -> if offset = 0us then $"[bp + si]" else $"[bp + si + %i{offset}]"
        | 0b011uy -> if offset = 0us then $"[bp + di]" else $"[bp + di + %i{offset}]"
        | 0b100uy -> if offset = 0us then $"[si]" else $"[si + %i{offset}]"
        | 0b101uy -> if offset = 0us then $"[di]" else $"[di + %i{offset}]"
        | 0b110uy -> if offset = 0us then $"[bp]" else $"[bp + %i{offset}]"
        | 0b111uy -> if offset = 0us then $"[bx]" else $"[bx + %i{offset}]"
        | b -> failwithf $"Unkown rm: %B{b}"
    let getOpcode (opcode : byte) extention =
        match (opcode >>>2) with
        | opcode 
            when opcode = MOV 
            || opcode = MOVE_IMMEDIATE_TO_MEMORY_OR_REGISTER 
            || opcode = MOVE_TO_FROM_ACCUMULATOR 
            || (opcode >>> 2) = MOV_IMMEDIATE_TO_REGISTER -> "mov"
        | opcode 
            when opcode = ADD
            || opcode = ADD_IMMEDIATE_TO_MEMORY_OR_REGISTER && extention = 0b000uy
            || opcode = ADD_IMMEDIATE_TO_ACCUMULATOR -> "add"
        | opcode 
            when opcode = SUB 
            || (opcode = SUB_IMMEDIATE_TO_MEMORY_OR_REGISTER && extention = 0b101uy) 
            || opcode  = SUB_IMMEDIATE_TO_ACCUMULATOR -> "sub"
        | opcode
            when opcode = CMP
            || (opcode = CMP_IMMEDIATE_TO_MEMORY_OR_REGISTER && extention = 0b111uy)
            || opcode = CMP_IMMEDIATE_TO_ACCUMULATOR -> "cmp"

        | opcode -> failwithf $"opcode '%06B{opcode} not yet supported"

    let getSourceAndDestination mapRegisters mapRMS d w reg rm displacement =
        let source =
            match d with
            | REG_IS_SOURCE -> mapRegisters w reg
            | REG_IS_DESTINATION -> mapRMS rm displacement
            | _ -> failwithf "unrechable code"

        let destination =
            match d with
            | REG_IS_SOURCE -> mapRMS rm displacement
            | REG_IS_DESTINATION -> mapRegisters w reg
            | _ -> failwithf "unrechable code"
        
        source, destination

    let getSourceAndDestinationBaseCase (bytes : byte[]) (i : byref<int>) d w ``mod`` reg rm = 
        let (source, destination) =
            match ``mod`` with
            | MEMORY_MODE_00_DISPLACEMENT ->
                printfn "MEMORY D0"
                
                let displacement = read16bits bytes[i+2] bytes[i+3]
                let source, destination = getSourceAndDestination registers rms00 d w reg rm displacement

                if rm = 0b110uy then
                    i <- i + 2

                (source, destination)

            | MEMORY_MODE_08_DISPLACEMENT ->
                printfn "MEMORY D8"
                let displacement = uint16 bytes[i+2]
                let source, destination = getSourceAndDestination registers rmsDisplaced d w reg rm displacement

                i <- i + 1

                (source, destination)
            | MEMORY_MODE_16_DISPLACEMENT -> 
                printfn "MEMORY D16"
                let displacement = read16bits bytes[i+2] bytes[i+3]
                let source, destination = getSourceAndDestination registers rmsDisplaced d w reg rm displacement

                i <- i + 2

                (source, destination)
            | REGISTER_TO_REGISTER_MODE ->
                printfn "REGISTER TO REGISTER"
                let source =
                    match d with
                    | REG_IS_DESTINATION -> registers w rm
                    | REG_IS_SOURCE -> registers w reg
                    | _ -> failwithf "unrechable code"

                let destination = 
                    match d with
                    | REG_IS_SOURCE ->  registers w rm
                    | REG_IS_DESTINATION -> registers w reg
                    | _ -> failwithf "unrechable code"
                
                source, destination
            | ``mod`` -> failwithf $"'%02B{``mod``}' mode is invalid"
        
        source,destination

    let getSourceAndDestinationImmediateCase (bytes : byte[]) (i : byref<int>) s w ``mod`` rm =
        let source, destination =
            match ``mod`` with
            | MEMORY_MODE_00_DISPLACEMENT ->
                printfn "IMM: MEMORY D0"
                let displacement = read16bits bytes[i+2] bytes[i+3]
                let destination =  rms00 rm displacement
                let source = 
                    if w then
                        read16bits bytes[i+4] bytes[i+5]
                    else
                        uint16 bytes[i+4]

                if rm = 0b110uy then
                    i <- i + 2

                source, destination

            | MEMORY_MODE_08_DISPLACEMENT ->
                printfn "IMM: MEMORY D8"
                let displacement = uint16 bytes[i+2]
                let destination = rmsDisplaced rm displacement
                let source = 
                    if w then
                        read16bits bytes[i+3] bytes[i+4]
                    else
                        uint16 bytes[i+3]

                i <- i + 1
                source, destination

            | MEMORY_MODE_16_DISPLACEMENT -> 
                printfn "IMM: MEMORY D16"
                let displacement = read16bits bytes[i+2] bytes[i+3]
                let destination = rmsDisplaced rm displacement
                let source = 
                    if w then
                        read16bits bytes[i+4] bytes[i+5]
                    else
                        uint16 bytes[i+4]

                i <- i + 2
                source, destination

            | REGISTER_TO_REGISTER_MODE ->
                printfn "IMM: REG to REG"
                let destination = registers w rm
                let source = 
                    if s = 0b0uy && w then
                        read16bits bytes[i+2] bytes[i+3]
                    else
                        uint16 bytes[i+2]

                source, destination
            
            | ``mod`` -> failwithf $"'%02B{``mod``}' mode is invalid"

        source,destination

    let baseCase (bytes : byte[]) (i : byref<int>) =
        let byte0 = bytes[i]
        let byte1 = bytes[i+1]

        let d = get7thBit byte0
        let w = get8thBit byte0 = 0b1uy
        let ``mod`` = getFirst2Bits byte1
        let reg =  get3thto5thBits byte1
        let rm = getLast3Bits byte1
        
        let opcode = getOpcode byte0 reg
        
        let (source, destination) = getSourceAndDestinationBaseCase bytes &i d w ``mod`` reg rm

        $"{opcode} {destination}, {source}"
            
    let accumulatorCase (bytes : byte[]) (i : byref<int>) = 
        let byte0 = bytes[i]
        let byte1 = bytes[i+1]

        let w = get8thBit byte0 = 0b1uy
        let d = get7thBit byte0 = 0b1uy
        let reg =  get3thto5thBits byte1
        let opcode = getOpcode byte0 reg
        
        let data = 
            if w then
                let data = read16bits bytes[i+1] bytes[i+2]
                i <- i + 1
                data
            else
                let data = uint16 bytes[i+1]
                data
        
        if d then
            $"{opcode} [{data}], ax"
        else
            $"{opcode} ax, [{data}]"

    let immediateMemoryRegister (bytes : byte[]) (i: byref<int>) =
        let byte0 = bytes[i]
        let byte1 = bytes[i+1]
        
        let ``mod`` = getFirst2Bits byte1
        let opcodeExtention = get3thto5thBits byte1
        let s = get7thBit byte0
        let w = get8thBit byte0 = 0b1uy
        let rm = getLast3Bits byte1
        
        let opcode = getOpcode byte0 opcodeExtention
        
        let source,destination = getSourceAndDestinationImmediateCase bytes &i s w ``mod`` rm

        if s= 0b0uy && w then
            i <- i + 2
            printfn $"sw={s}{w}"
            $"{opcode} {destination}, word %i{source}"
        else
            printfn $"sw={s}{w}"
            i <- i + 1
            $"{opcode} {destination}, byte %i{source}"

    let decode (bytes : byte[]) =
        let sb = StringBuilder()
        sb.AppendLine("bits 16\n") |> ignore
        let mutable i = 0
        while i < bytes.Length - 1 do
            
            let byte0 = bytes[i]
            PrintTools.printByteWithPrefix $"[{i}]" byte0
            match (byte0 >>> 2) with
            | opcode 
                when opcode = MOV 
                || opcode = ADD
                || opcode = SUB
                || opcode = CMP ->

                let asm = baseCase bytes &i
                sb.AppendLine(asm) |> ignore

            | opcode 
                when opcode = MOVE_IMMEDIATE_TO_MEMORY_OR_REGISTER 
                || opcode = ADD_IMMEDIATE_TO_MEMORY_OR_REGISTER 
                || opcode = SUB_IMMEDIATE_TO_MEMORY_OR_REGISTER
                || opcode = CMP_IMMEDIATE_TO_MEMORY_OR_REGISTER ->

                let asm = immediateMemoryRegister bytes &i
                sb.AppendLine(asm) |> ignore

            | opcode 
                when opcode = MOVE_TO_FROM_ACCUMULATOR 
                || opcode = ADD_IMMEDIATE_TO_ACCUMULATOR
                || opcode = SUB_IMMEDIATE_TO_ACCUMULATOR 
                || opcode = CMP_IMMEDIATE_TO_ACCUMULATOR ->

                let asm = accumulatorCase bytes &i
                sb.AppendLine(asm) |> ignore

            | opcode when (opcode >>> 2) = MOV_IMMEDIATE_TO_REGISTER ->
                
                printfn "IMMEDIATE TO REGISTER"
                let w = get5thBit byte0 = 0b1uy
                let reg = getLast3Bits byte0
                let destination = registers w reg

                if w then
                    let displacement =
                        read16bits bytes[i+1] bytes[i+2]

                    sb.AppendLine($"mov {destination}, %i{displacement}") |> ignore
                
                    i <- i + 1
                else
                    let displacement = int8 bytes[i+1]

                    sb.AppendLine($"mov {destination}, %i{displacement}") |> ignore
            | opcode when opcode = JMP_EQUAL_ZERO_OR_NEGATION ->
                let jump = sbyte bytes[i + 1]
                let negated = get8thBit byte0
                if negated = 0b1uy then
                    sb.AppendLine($"jnz {jump}") |> ignore
                else
                    sb.AppendLine($"jz {jump}") |> ignore
            | opcode when opcode = JPM_ON_LESS_OR_LESS_OR_EQUAL_OR_NEGATION ->
                let e = get7thBit byte0
                let jump = sbyte bytes[i + 1]
                let negated = get8thBit byte0
                if e = 0b1uy then
                    if negated = 0b1uy then
                        sb.AppendLine($"jnle {jump}") |> ignore
                    else
                        sb.AppendLine($"jle {jump}") |> ignore
                else
                    if negated = 0b1uy then
                        sb.AppendLine($"jnl {jump}") |> ignore
                    else
                        sb.AppendLine($"jl {jump}") |> ignore
            | opcode when opcode = JMP_ON_BELOW_OR_NEGATION ->
                let jump= sbyte bytes[i + 1]
                let negated = get8thBit byte0
                if negated = 0b1uy then
                    sb.AppendLine($"jnb {jump}") |> ignore
                else
                    sb.AppendLine($"jb {jump}") |> ignore
            | opcode when opcode = JMP_ON_BELOW_OR_EQUAL_OR_NEGATION ->
                let jump= sbyte bytes[i + 1]
                let negated = get8thBit byte0

                if negated = 0b1uy then
                    sb.AppendLine($"jnbe {jump}") |> ignore
                else
                    sb.AppendLine($"jbe {jump}") |> ignore
            | opcode when opcode = JMP_ON_PARITY_OR_NEGATION ->
                let jump= sbyte bytes[i + 1]
                let negated = get8thBit byte0

                if negated = 0b1uy then
                    sb.AppendLine($"jnp {jump}") |> ignore
                else
                    sb.AppendLine($"jp {jump}") |> ignore
            | opcode when opcode = JMP_ON_OVERFLOW_OR_NEGATION ->
                let jump= sbyte bytes[i + 1]
                let negated = get8thBit byte0

                if negated = 0b1uy then
                    sb.AppendLine($"jno {jump}") |> ignore
                else
                    sb.AppendLine($"jo {jump}") |> ignore
            | opcode when opcode = JMP_ON_SIGN_OR_NEGATION ->
                let jump= sbyte bytes[i + 1]
                let negated = get8thBit byte0

                if negated = 0b1uy then
                    sb.AppendLine($"jns {jump}") |> ignore
                else
                    sb.AppendLine($"js {jump}") |> ignore

            | opcode when opcode = LOOP ->
                let seventh = get7thBit byte0
                let lowbit = get8thBit byte0
                let jump= sbyte bytes[i + 1]

                match seventh, lowbit with
                | 0b1uy, 0b0uy -> sb.AppendLine($"loop {jump}") |> ignore
                | 0b0uy, 0b1uy -> sb.AppendLine($"loopz {jump}") |> ignore
                | 0b0uy, 0b0uy -> sb.AppendLine($"loopnz {jump}") |> ignore
                | 0b1uy, 0b1uy -> sb.AppendLine($"jcxz {jump}") |> ignore
                | _,_-> failwith "unrecheable code"

            | opcode -> failwithf $"opcode '%06B{opcode}' not yet supported."

            i <- i + 2

        sb.ToString()

    let decodeFile source destination = 
        File.ReadAllBytes(source)
        |> decode 
        |> fun text -> File.WriteAllText(destination, text)

Decoding8086.decodeFile "./listing_0037_single_register_mov" "./decoded_single_register_move.asm"
Decoding8086.decodeFile "./listing_0038_many_register_mov" "./decoded_many_register_move.asm"
Decoding8086.decodeFile "./listing_0039_more_movs" "./decoded_more_movs.asm"
Decoding8086.decodeFile "./listing_0040_challenge_movs" "./decoded_challenges_movs.asm"
Decoding8086.decodeFile "./listing_0041_add_sub_cmp_jnz" "./decoded_add_sub_cmp_jnz.asm"

File.ReadAllBytes("./listing_0041_add_sub_cmp_jnz") |> PrintTools.printBinary
