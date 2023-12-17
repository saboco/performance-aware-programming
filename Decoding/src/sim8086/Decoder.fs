module sim8086.Decoder

open System.Text
open System.IO

[<RequireQualifiedAccess>]
type Mode =
    | Memory0
    | Memory0D16
    | MemoryD8
    | MemoryD16
    | Register

[<RequireQualifiedAccess>]
type Register = 
    | AX | AL
    | CX | CL
    | DX | DL
    | BX | BL
    | SP | AH
    | BP | CH
    | SI | DH
    | DI | BH

type Address =
    | Register of Register
    | DirectAddress of int
    | Formula of Address : Register [] * Offset : int

type Destination = Address
type Source = Address

[<RequireQualifiedAccess>]
type Instruction =
    | Mov of Destination * Source
    | MovImmediateRegisterMemory of Destination * Value : int * IsWord : bool
    | Add of Destination * Source
    | AddImmediateToRegisterMemory of Destination * Value : int * IsWord : bool
    | Sub of Destination * Source
    | SubImmediateToRegisterMemory of Destination * Value : int * IsWord : bool
    | Cmp of Destination * Source
    | CmpImmediateToRegisterMemory of Destination * Value : int * IsWord : bool
    | Jz of int
    | Jnz of int
    | Jl of int
    | Jnl of int
    | Jle of int
    | Jnle of int
    | Jb of int
    | Jnb of int
    | Jbe of int
    | Jnbe of int
    | Jp of int
    | Jnp of int
    | Jo of int
    | Jno of int
    | Js of int
    | Jns of int
    | Loop of int
    | Loopz of int
    | Loopnz of int
    | Jcxz of int

let isWord w = w = 0b1uy
let signExtend s = s = 0b1uy
let destinationIsRegister d = d = 0b1uy
let sourceIsRegister d = d = 0b0uy

module Registers = 
    let fromId id w =
        match id with
        | 0b000uy -> if isWord w then Register.AX else Register.AL
        | 0b001uy -> if isWord w then Register.CX else Register.CL
        | 0b010uy -> if isWord w then Register.DX else Register.DL
        | 0b011uy -> if isWord w then Register.BX else Register.BL
        | 0b100uy -> if isWord w then Register.SP else Register.AH
        | 0b101uy -> if isWord w then Register.BP else Register.CH
        | 0b110uy -> if isWord w then Register.SI else Register.DH
        | 0b111uy -> if isWord w then Register.DI else Register.BH
        | b -> failwithf $"Unknown register: %B{b}"

    let effectiveAddress rm =
        match rm with
        | 0b000uy -> [|Register.BX; Register.SI|]
        | 0b001uy -> [|Register.BX; Register.DI|]
        | 0b010uy -> [|Register.BP; Register.SI|]
        | 0b011uy -> [|Register.BP; Register.DI|]
        | 0b100uy -> [|Register.SI|]
        | 0b101uy -> [|Register.DI|]
        | 0b110uy -> [|Register.BP|]
        | 0b111uy -> [|Register.BX|]
        | b -> failwithf $"Unknown effective address case: %08B{b}"
    
    let sprint register = 
        match register with
        | Register.AX -> "ax" 
        | Register.AL -> "al"
        | Register.CX -> "cx" 
        | Register.CL -> "cl"
        | Register.DX -> "dx" 
        | Register.DL -> "dl"
        | Register.BX -> "bx" 
        | Register.BL -> "bl"
        | Register.SP -> "sp" 
        | Register.AH -> "ah"
        | Register.BP -> "bp" 
        | Register.CH -> "ch"
        | Register.SI -> "si" 
        | Register.DH -> "dh"
        | Register.DI -> "di" 
        | Register.BH -> "bh"

    let sprintEffectiveAddress explicitSize isWord (registers : Register[]) (offset : int) =
        let sb = StringBuilder() 
        
        if explicitSize then
            if isWord then
                sb.Append("word [") |> ignore
            else
                sb.Append("byte [") |> ignore
        else
            sb.Append("[") |> ignore
                
        let mutable isFirst = true
        for register in registers do
            if not isFirst then
                sb.Append(" + ") |> ignore
            sb.Append(sprint register) |> ignore
            isFirst <- false

        if offset > 0 then
            sb.Append($" + {offset}") |> ignore
        elif offset < 0 then
            sb.Append($" - {-offset}") |> ignore
        
        sb.Append("]") |> ignore
        sb.ToString()
    
    let sprintAddress explicitSize isWord (adr : Address) =
        match adr with
        | Register reg -> sprint reg
        | DirectAddress adr -> 
            if explicitSize then
                if isWord then
                    $"word [{adr}]"
                else
                    $"byte [{adr}]"
            else
                $"[{adr}]"
        | Formula (reg,offset) -> sprintEffectiveAddress explicitSize isWord reg offset

    let printInstructions (instructions : ResizeArray<Instruction>) =
        let sb = StringBuilder()
        sb.AppendLine("bits 16") |> ignore
        sb.AppendLine() |> ignore
        for instruction in instructions do
            match instruction with
            | Instruction.Mov (destination, source) ->
                let destination = sprintAddress false false destination
                let source = sprintAddress false false source
                sb.AppendLine($"mov {destination}, {source}") |> ignore
            | Instruction.MovImmediateRegisterMemory (destination, value, isWord) ->
                let destination = sprintAddress true isWord destination
                sb.AppendLine($"mov {destination}, {value}") |> ignore
            | Instruction.Add (destination, source) ->
                let destination = sprintAddress false false destination
                let source = sprintAddress false false source
                sb.AppendLine($"add {destination}, {source}") |> ignore
            | Instruction.AddImmediateToRegisterMemory (destination, value, isWord) ->
                let destination = sprintAddress true isWord destination
                sb.AppendLine($"add {destination}, {value}") |> ignore
            | Instruction.Sub (destination, source) ->
                let destination = sprintAddress false false destination
                let source = sprintAddress false false source
                sb.AppendLine($"sub {destination}, {source}") |> ignore
            | Instruction.SubImmediateToRegisterMemory (destination, value, isWord) ->
                let destination = sprintAddress true isWord destination
                sb.AppendLine($"sub {destination}, {value}") |> ignore
            | Instruction.Cmp (destination, source) ->
                let destination = sprintAddress false false destination
                let source = sprintAddress false false source
                sb.AppendLine($"cmp {destination}, {source}") |> ignore
            | Instruction.CmpImmediateToRegisterMemory (destination, value, isWord) ->
                let destination = sprintAddress true isWord destination
                sb.AppendLine($"cmp {destination}, {value}") |> ignore
            | Instruction.Jz jump -> sb.AppendLine($"jz {jump}") |> ignore
            | Instruction.Jnz jump -> sb.AppendLine($"jnz {jump}") |> ignore
            | Instruction.Jl jump -> sb.AppendLine($"jl {jump}") |> ignore
            | Instruction.Jnl jump -> sb.AppendLine($"jnl {jump}") |> ignore
            | Instruction.Jle jump -> sb.AppendLine($"jle {jump}") |> ignore
            | Instruction.Jnle jump -> sb.AppendLine($"jnle {jump}") |> ignore
            | Instruction.Jb jump -> sb.AppendLine($"jb {jump}") |> ignore
            | Instruction.Jnb jump -> sb.AppendLine($"jnb {jump}") |> ignore
            | Instruction.Jbe jump -> sb.AppendLine($"jbe {jump}") |> ignore
            | Instruction.Jnbe jump -> sb.AppendLine($"jnbe {jump}") |> ignore
            | Instruction.Jp jump -> sb.AppendLine($"jp {jump}") |> ignore
            | Instruction.Jnp jump -> sb.AppendLine($"jnp {jump}") |> ignore
            | Instruction.Jo jump -> sb.AppendLine($"jo {jump}") |> ignore
            | Instruction.Jno jump -> sb.AppendLine($"jno {jump}") |> ignore
            | Instruction.Js jump -> sb.AppendLine($"js {jump}") |> ignore
            | Instruction.Jns jump -> sb.AppendLine($"jns {jump}") |> ignore
            | Instruction.Loop jump -> sb.AppendLine($"loop {jump}") |> ignore
            | Instruction.Loopz jump -> sb.AppendLine($"loopz {jump}") |> ignore
            | Instruction.Loopnz jump -> sb.AppendLine($"loopnz {jump}") |> ignore
            | Instruction.Jcxz jump -> sb.AppendLine($"jcxz {jump}") |> ignore
        sb.ToString()

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
    let get3thTo5thBits byte = (byte >>> 3) &&&  0b00000111uy
    let getLast3Bits byte = byte &&& 0b00000111uy
    let get5thBit byte = (byte >>> 3) &&& 0b00000001uy
 
    // instruction 
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
    let [<Literal>] MOVE_IMMEDIATE_TO_REGISTER_MEMORY = 0b1100011uy
    let [<Literal>] MOV_IMMEDIATE_TO_REGISTER = 0b1011uy
    let [<Literal>] MOVE_TO_FROM_ACCUMULATOR = 0b101000uy
    let [<Literal>] ADD = 0b000000uy
    let [<Literal>] ADD_IMMEDIATE_TO_REGISTER_MEMORY = 0b100000uy
    let [<Literal>] ADD_IMMEDIATE_TO_ACCUMULATOR = 0b0000010uy
    let [<Literal>] SUB = 0b001010uy
    let [<Literal>] SUB_IMMEDIATE_TO_REGISTER_MEMORY = 0b100000uy
    let [<Literal>] SUB_IMMEDIATE_TO_ACCUMULATOR = 0b0010110uy
    let [<Literal>] CMP = 0b001110uy
    let [<Literal>] CMP_IMMEDIATE_TO_REGISTER_MEMORY = 0b100000uy
    let [<Literal>] CMP_IMMEDIATE_TO_ACCUMULATOR = 0b0011110uy
    let [<Literal>] JMP_EQUAL_ZERO_OR_NEGATION = 0b0111010uy
    let [<Literal>] JPM_ON_LESS_OR_NEGATION = 0b0111110uy
    let [<Literal>] JPM_ON_LESS_OR_EQUAL_OR_NEGATION = 0b0111111uy
    let [<Literal>] JMP_ON_BELOW_OR_NEGATION = 0b0111001uy
    let [<Literal>] JMP_ON_BELOW_OR_EQUAL_OR_NEGATION = 0b0111011uy
    let [<Literal>] JMP_ON_PARITY_OR_NEGATION = 0b0111101uy
    let [<Literal>] JMP_ON_OVERFLOW_OR_NEGATION = 0b0111000uy
    let [<Literal>] JMP_ON_SIGN_OR_NEGATION = 0b0111100uy
    let [<Literal>] LOOP_CX_TIMES_OR_ON_CX_ZERO = 0b1110001uy
    let [<Literal>] LOOP_WHILE_ZERO_OR_NEGATION = 0b1110000uy

    // memory modes
    let [<Literal>] MEMORY_MODE_00_DISPLACEMENT = 0b00uy
    let [<Literal>] MEMORY_MODE_08_DISPLACEMENT = 0b01uy
    let [<Literal>] MEMORY_MODE_16_DISPLACEMENT = 0b10uy
    let [<Literal>] REGISTER_TO_REGISTER_MODE = 0b11uy

    // REG operand
    let [<Literal>] REG_IS_SOURCE = 0b0uy
    let [<Literal>] REG_IS_DESTINATION = 0b1uy

    let read16bits (bl : byte) (bh : byte) isSigned : int = 
        let bl = uint16 bl
        let bh = uint16 bh
        
        if isSigned then
            int (int16 ((bh <<< 8) |||  bl))
        else    
            int ((bh <<< 8) |||  bl)

    let read8bits (b : byte) s =
        let b = 
            if s then
                sbyte b |> int
            else
                int b
        printfn $"[read8bits] %i{b}"
        b
        
    let parseMemoryMode ``mod`` rm =
        if (``mod`` = 0b00uy && rm = 0b110uy) then
            Mode.Memory0D16
        elif ``mod`` = 0b01uy then
            Mode.MemoryD8
        elif ``mod`` = 0b10uy then
            Mode.MemoryD16
        elif ``mod`` = 0b11uy then
            Mode.Register
        else
            Mode.Memory0
            
    let baseCase (stream : byte[]) at d w ``mod`` reg rm =
        let memoryMode = parseMemoryMode ``mod`` rm
        let isWord = isWord w
        let streamOffset = 2

        let source, destination, streamOffset = 
            match memoryMode with
            | Mode.Memory0 ->
                let register = Registers.fromId reg w |> Register
                let address =
                    let registers = Registers.effectiveAddress rm
                    let offset = 0
                    Formula (registers, offset)
                
                if sourceIsRegister d then
                    register, address, streamOffset
                else
                    address, register, streamOffset

            | Mode.Memory0D16 ->
                let register = Registers.fromId reg w |> Register
                let address =
                    DirectAddress (read16bits (stream[at + streamOffset]) (stream[at + streamOffset + 1]) isWord)
                
                if sourceIsRegister d then
                    register, address, streamOffset + 2
                else
                    address, register, streamOffset + 2
            | Mode.MemoryD8 ->
                let register = Registers.fromId reg w |> Register
                let address =
                    let registers = Registers.effectiveAddress rm
                    let offset = read8bits stream[at + streamOffset] isWord
                    Formula (registers, offset)
                
                if sourceIsRegister d then
                    register, address, streamOffset + 1
                else
                    address, register, streamOffset + 1
            | Mode.MemoryD16 ->
                let register = Registers.fromId reg w |> Register
                let address =
                    let registers = Registers.effectiveAddress rm
                    let offset = read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord
                    Formula (registers, offset)
                
                if sourceIsRegister d then
                    register, address, streamOffset + 2
                else
                    address, register, streamOffset + 2
            | Mode.Register ->
                let destination =
                    if destinationIsRegister d then
                        Registers.fromId reg w |> Register
                    else
                        Registers.fromId rm w |> Register
                    
                let source = 
                    if sourceIsRegister d then
                        Registers.fromId reg w |> Register
                    else
                        Registers.fromId rm w |> Register
                    
                source, destination, streamOffset
        
        source, destination, streamOffset

    let jumpSwitch (stream : byte[]) at op opNegated : Instruction =
        let n = get8thBit stream[at]
        let jump = int stream[at + 1]
        
        if n = 0b0uy then
            op jump
        else
            opNegated jump

    let decode (stream : byte[]) =
        let mutable i = 0
        let instructions = ResizeArray()
        while i < stream.Length - 1 do
            
            PrintTools.printByteWithPrefix $"[{i}]" stream[i]
            
            // treating all cases individually to be able to see patterns
            match stream[i] with
            | opcode when (opcode >>> 2) = MOV ->
                let d = get7thBit stream[i]
                let w = get8thBit stream[i]
                let ``mod`` = getFirst2Bits stream[i+1]
                let reg = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let source, destination, streamOffset = baseCase stream i d w ``mod`` reg rm

                instructions.Add(Instruction.Mov (destination,source))
                i <- i + streamOffset

            | opcode when (opcode >>> 1) = MOVE_IMMEDIATE_TO_REGISTER_MEMORY ->
                let w = get8thBit stream[i]
                let isWord = isWord w
                let ``mod`` = getFirst2Bits stream[i+1]
                let _opcodeExtension = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]
                
                let memoryMode = parseMemoryMode ``mod`` rm
                
                let streamOffset = 2
                
                let destination, streamOffset = 
                    match memoryMode with
                    | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0), streamOffset
                    | Mode.Memory0D16 -> DirectAddress (read16bits (stream[i + streamOffset]) (stream[i + streamOffset + 1]) isWord), streamOffset + 2
                    | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[i + streamOffset] isWord), streamOffset + 1
                    | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[i + streamOffset] stream[i + streamOffset + 1] isWord), streamOffset + 2
                    | Mode.Register -> Register (Registers.fromId rm w), streamOffset

                let data, streamOffset = 
                    if isWord then
                        read16bits stream[i + streamOffset] stream[i + streamOffset + 1] isWord, streamOffset + 2
                    else
                        read8bits stream[i + streamOffset] isWord, streamOffset + 1

                instructions.Add(Instruction.MovImmediateRegisterMemory (destination, data, isWord))

                i <- i + streamOffset
            | opcode when (opcode >>> 4) = MOV_IMMEDIATE_TO_REGISTER ->
                let w = get5thBit stream[i]
                let isWord = isWord w
                let reg = getLast3Bits stream[i]

                let streamOffset = 1
                let data, streamOffset =
                    if isWord then
                        read16bits stream[i + 2 + streamOffset + 1] stream[i + streamOffset + 2] isWord, streamOffset + 2
                    else
                        int stream[i + streamOffset + 1], streamOffset + 1

                instructions.Add(Instruction.MovImmediateRegisterMemory(Register (Registers.fromId reg w), data, isWord))
                i <- i + streamOffset

            | opcode when (opcode >>> 2) = MOVE_TO_FROM_ACCUMULATOR ->
                let toMemory = (get7thBit stream[i]) = 0b1uy
                let w = get8thBit stream[i]
                let isWord = isWord w
                let streamOffset = 1
                let memory, streamOffset = 
                    if isWord then
                        DirectAddress (read16bits stream[i+streamOffset] stream[i+streamOffset+1] isWord), streamOffset + 2
                    else
                        DirectAddress (int stream[i+streamOffset]), streamOffset + 1

                let register = Register Register.AX
                let operation =
                    if toMemory then
                        Instruction.Mov (memory, register)
                    else
                        Instruction.Mov (register, memory)
                
                instructions.Add(operation)

                i <- i + streamOffset
            | opcode when (opcode >>> 2) = ADD ->
                let d = get7thBit stream[i]
                let w = get8thBit stream[i]
                let ``mod`` = getFirst2Bits stream[i+1]
                let reg = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let source, destination, streamOffset = baseCase stream i d w ``mod`` reg rm
                
                instructions.Add(Instruction.Add (destination,source))
                i <- i + streamOffset
            | opcode when (opcode >>> 2) = SUB ->
                let d = get7thBit stream[i]
                let w = get8thBit stream[i]
                let ``mod`` = getFirst2Bits stream[i+1]
                let reg = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let source, destination, streamOffset = baseCase stream i d w ``mod`` reg rm
                
                instructions.Add(Instruction.Sub (destination,source))
                i <- i + streamOffset
            | opcode when (opcode >>> 2) = CMP ->
                let d = get7thBit stream[i]
                let w = get8thBit stream[i]
                let ``mod`` = getFirst2Bits stream[i+1]
                let reg = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let source, destination, streamOffset = baseCase stream i d w ``mod`` reg rm
                
                instructions.Add(Instruction.Cmp (destination,source))
                i <- i + streamOffset
            | opcode when (opcode >>> 2) = ADD_IMMEDIATE_TO_REGISTER_MEMORY && (get3thTo5thBits stream[i+1] = 0b000uy) ->
                let s = get7thBit stream[i]
                let w = get8thBit stream[i]
                let isWord = isWord w
                let signExtend = signExtend s
                let ``mod`` = getFirst2Bits stream[i+1]
                let _opcodeExtension = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let memoryMode = parseMemoryMode ``mod`` rm
                let streamOffset = 2
                printfn $"ADD IMM %A{memoryMode}"
                
                let destination, streamOffset = 
                    match memoryMode with
                    | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0), streamOffset
                    | Mode.Memory0D16 -> DirectAddress (read16bits (stream[i + streamOffset]) (stream[i + streamOffset + 1]) isWord), streamOffset + 2
                    | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[i + streamOffset] isWord), streamOffset + 1
                    | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[i + streamOffset] stream[i + streamOffset + 1] isWord), streamOffset + 2
                    | Mode.Register -> Register (Registers.fromId rm w), streamOffset
                
                let data, streamOffset = 
                    if not signExtend && isWord then
                        read16bits stream[i + streamOffset] stream[i + streamOffset + 1] signExtend, streamOffset + 2
                    else
                        read8bits (stream[i + streamOffset]) signExtend, streamOffset + 1

                instructions.Add(Instruction.AddImmediateToRegisterMemory(destination, data, isWord))
                i <- i + streamOffset
            | opcode when (opcode >>> 2) = SUB_IMMEDIATE_TO_REGISTER_MEMORY && (get3thTo5thBits stream[i+1] = 0b101uy) ->
                
                let s = get7thBit stream[i]
                let w = get8thBit stream[i]
                let isWord = isWord w
                let signExtend = signExtend s
                let ``mod`` = getFirst2Bits stream[i+1]
                let _opcodeExtension = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let memoryMode = parseMemoryMode ``mod`` rm
                let streamOffset = 2
                printfn $"SUB IMM %A{memoryMode}"
                
                let destination, streamOffset = 
                    match memoryMode with
                    | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0), streamOffset
                    | Mode.Memory0D16 -> DirectAddress (read16bits (stream[i + streamOffset]) (stream[i + streamOffset + 1]) signExtend), streamOffset + 2
                    | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[i + streamOffset] signExtend), streamOffset + 1
                    | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[i + streamOffset] stream[i + streamOffset + 1] signExtend), streamOffset + 2
                    | Mode.Register -> Register (Registers.fromId rm w), streamOffset
                
                let data, streamOffset = 
                    if not signExtend && isWord then
                        read16bits stream[i + streamOffset] stream[i + streamOffset + 1] signExtend, streamOffset + 2
                    else
                        read8bits stream[i + streamOffset] signExtend, streamOffset + 1

                instructions.Add(Instruction.SubImmediateToRegisterMemory(destination, data, isWord))
                i <- i + streamOffset

            | opcode when (opcode >>> 2) = CMP_IMMEDIATE_TO_REGISTER_MEMORY && (get3thTo5thBits stream[i+1] = 0b111uy) ->
                
                let s = get7thBit stream[i]
                let w = get8thBit stream[i]
                let isWord = isWord w
                let signExtend = signExtend s
                let ``mod`` = getFirst2Bits stream[i+1]
                let _opcodeExtension = get3thTo5thBits stream[i+1]
                let rm = getLast3Bits stream[i+1]

                let memoryMode = parseMemoryMode ``mod`` rm
                let streamOffset = 2
                printfn $"CMP IMM %A{memoryMode}"

                let destination, streamOffset = 
                    match memoryMode with
                    | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0), streamOffset
                    | Mode.Memory0D16 -> DirectAddress (read16bits (stream[i + streamOffset]) (stream[i + streamOffset + 1]) signExtend), streamOffset + 2
                    | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[i + streamOffset] signExtend), streamOffset + 1
                    | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[i + streamOffset] stream[i + streamOffset + 1] signExtend), streamOffset + 2
                    | Mode.Register -> Register (Registers.fromId rm w), streamOffset
                
                let data, streamOffset = 
                    if not signExtend && isWord then
                        read16bits stream[i + streamOffset] stream[i + streamOffset + 1] signExtend, streamOffset + 2
                    else
                        read8bits stream[i + streamOffset] signExtend, streamOffset + 1

                instructions.Add(Instruction.CmpImmediateToRegisterMemory(destination, data, isWord))
                i <- i + streamOffset

            | opcode when (opcode >>> 1) = ADD_IMMEDIATE_TO_ACCUMULATOR ->
                let isWord = isWord (get8thBit stream[i])
                let streamOffset = 1
                let data, streamOffset =
                    if isWord then
                        read16bits stream[i+streamOffset] stream[i+streamOffset+1] isWord, streamOffset + 2
                    else
                        read8bits stream[i+streamOffset] isWord, streamOffset + 1
                     
                if isWord then
                    instructions.Add(Instruction.AddImmediateToRegisterMemory(Register Register.AX, data, isWord))
                else
                    instructions.Add(Instruction.AddImmediateToRegisterMemory(Register Register.AL, data, isWord))

                i <- i + streamOffset
            | opcode when (opcode >>> 1) = SUB_IMMEDIATE_TO_ACCUMULATOR ->
                let isWord = isWord (get8thBit stream[i])
                let streamOffset = 1
                let data, streamOffset =
                    if isWord then
                        read16bits stream[i+streamOffset] stream[i+streamOffset+1] isWord, streamOffset + 2
                    else
                        read8bits stream[i+streamOffset] isWord, streamOffset + 1
                
                if isWord then
                    instructions.Add(Instruction.SubImmediateToRegisterMemory(Register Register.AX, data, isWord))
                else
                    instructions.Add(Instruction.SubImmediateToRegisterMemory(Register Register.AL, data, isWord))

                i <- i + streamOffset
            | opcode when (opcode >>> 1) = CMP_IMMEDIATE_TO_ACCUMULATOR ->
                let isWord = isWord (get8thBit stream[i])
                let streamOffset = 1
                let data, streamOffset =
                    if isWord then
                        read16bits stream[i+streamOffset] stream[i+streamOffset+1] isWord, streamOffset + 2
                    else
                        read8bits stream[i+streamOffset] isWord, streamOffset + 1
                
                if isWord then
                    instructions.Add(Instruction.CmpImmediateToRegisterMemory(Register Register.AX, data, isWord))
                else
                    instructions.Add(Instruction.CmpImmediateToRegisterMemory(Register Register.AL, data, isWord))

                i <- i + streamOffset
            | opcode when (opcode >>> 1) = JMP_EQUAL_ZERO_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jz Instruction.Jnz
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JPM_ON_LESS_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jl Instruction.Jnl
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JPM_ON_LESS_OR_EQUAL_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jle Instruction.Jnle
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JMP_ON_BELOW_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jb Instruction.Jnb
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JMP_ON_BELOW_OR_EQUAL_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jbe Instruction.Jnbe
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JMP_ON_PARITY_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jp Instruction.Jnp
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JMP_ON_OVERFLOW_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Jo Instruction.Jno
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = JMP_ON_SIGN_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Js Instruction.Jns
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = LOOP_CX_TIMES_OR_ON_CX_ZERO ->
                let jump = jumpSwitch stream i Instruction.Loop Instruction.Jcxz
                instructions.Add(jump)
                i <- i + 2
            | opcode when (opcode >>> 1) = LOOP_WHILE_ZERO_OR_NEGATION ->
                let jump = jumpSwitch stream i Instruction.Loopnz Instruction.Loopz
                instructions.Add(jump)
                i <- i + 2
            | opcode -> failwithf $"opcode '%06B{opcode}' not yet supported."

        instructions
        
    let decodeFile source destination = 
        File.ReadAllBytes(source)
        |> decode 
        |> Registers.printInstructions
        |> fun text -> File.WriteAllText(destination, text)