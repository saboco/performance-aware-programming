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
    | ES | CS | SS | DS // segment registers
    | IP // instruction pointer

type Address =
    | Register of Register
    | DirectAddress of Index : int * IsWord : bool
    | Formula of Address : Register [] * Offset : int * IsWord : bool

type Destination = Address
type Source = Address

[<RequireQualifiedAccess>]
type Instruction =
    | Mov of Destination * Source
    | MovImmediateRegisterMemory of Destination * Value : int 
    | Add of Destination * Source
    | AddImmediateToRegisterMemory of Destination * Value : int
    | Sub of Destination * Source
    | SubImmediateToRegisterMemory of Destination * Value : int
    | Cmp of Destination * Source
    | CmpImmediateToRegisterMemory of Destination * Value : int
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
    b
    
let write16Bits (arr : byte[]) idxLo idxHi data =
    let data = int16 data
    arr[idxLo] <- byte data
    arr[idxHi] <- byte (data >>> 8)

let write8bits (arr : byte[]) idx data= arr[idx] <- byte data
        
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
        | Register.ES -> "es"
        | Register.CS -> "cs"
        | Register.SS -> "ss"
        | Register.DS -> "ds"
        | Register.IP -> "ip"

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
    
    let sprintAddress explicitSize (adr : Address) =
        match adr with
        | Register reg -> sprint reg
        | DirectAddress (adr, isWord) -> 
            if explicitSize then
                if isWord then
                    $"word [{adr}]"
                else
                    $"byte [{adr}]"
            else
                $"[{adr}]"
        | Formula (reg,offset, isWord) -> sprintEffectiveAddress explicitSize isWord reg offset

    let sprintInstruction (instruction : Instruction) =
        match instruction with
        | Instruction.Mov (destination, source) ->
            let destination = sprintAddress false destination
            let source = sprintAddress false source
            $"mov {destination}, {source}"
        | Instruction.MovImmediateRegisterMemory (destination, value) ->
            let destination = sprintAddress true destination
            $"mov {destination}, {value}"
        | Instruction.Add (destination, source) ->
            let destination = sprintAddress false destination
            let source = sprintAddress false source
            $"add {destination}, {source}"
        | Instruction.AddImmediateToRegisterMemory (destination, value) ->
            let destination = sprintAddress true destination
            $"add {destination}, {value}"
        | Instruction.Sub (destination, source) ->
            let destination = sprintAddress false destination
            let source = sprintAddress false source
            $"sub {destination}, {source}"
        | Instruction.SubImmediateToRegisterMemory (destination, value) ->
            let destination = sprintAddress true destination
            $"sub {destination}, {value}"
        | Instruction.Cmp (destination, source) ->
            let destination = sprintAddress false destination
            let source = sprintAddress false source
            $"cmp {destination}, {source}"
        | Instruction.CmpImmediateToRegisterMemory (destination, value) ->
            let destination = sprintAddress true destination
            $"cmp {destination}, {value}"
        | Instruction.Jz jump -> $"jz {jump}"
        | Instruction.Jnz jump -> $"jnz {jump}"
        | Instruction.Jl jump -> $"jl {jump}"
        | Instruction.Jnl jump -> $"jnl {jump}"
        | Instruction.Jle jump -> $"jle {jump}"
        | Instruction.Jnle jump -> $"jnle {jump}"
        | Instruction.Jb jump -> $"jb {jump}"
        | Instruction.Jnb jump -> $"jnb {jump}"
        | Instruction.Jbe jump -> $"jbe {jump}"
        | Instruction.Jnbe jump -> $"jnbe {jump}"
        | Instruction.Jp jump -> $"jp {jump}"
        | Instruction.Jnp jump -> $"jnp {jump}"
        | Instruction.Jo jump -> $"jo {jump}"
        | Instruction.Jno jump -> $"jno {jump}"
        | Instruction.Js jump -> $"js {jump}"
        | Instruction.Jns jump -> $"jns {jump}"
        | Instruction.Loop jump -> $"loop {jump}"
        | Instruction.Loopz jump -> $"loopz {jump}"
        | Instruction.Loopnz jump -> $"loopnz {jump}"
        | Instruction.Jcxz jump -> $"jcxz {jump}"
        
    let printInstructions (instructions : Instruction[]) =
        let sb = StringBuilder()
        sb.AppendLine("bits 16") |> ignore
        sb.AppendLine() |> ignore
        for instruction in instructions do
            sb.AppendLine(sprintInstruction instruction) |> ignore
        sb.ToString()

let get7thBit byte = (byte >>> 1) &&& 0b00000001uy
let get8thBit byte = byte &&& 0b00000001uy
let getFirst2Bits byte = (byte >>> 6) &&& 0b00000011uy
let get3thTo5thBits byte = (byte >>> 3) &&&  0b00000111uy
let get4thAnd5thBits byte = (byte >>> 3) &&&  0b00000011uy
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
let [<Literal>] MOVE_REGISTER_MEMORY_TO_FROM_SEGMENT_REGISTER = 0b100011uy
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
        
let baseCase (stream : byte[]) at =
    let d = get7thBit stream[at]
    let w = get8thBit stream[at]
    let ``mod`` = getFirst2Bits stream[at+1]
    let reg = get3thTo5thBits stream[at+1]
    let rm = getLast3Bits stream[at+1]
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
                Formula (registers, offset, isWord)
            
            if sourceIsRegister d then
                register, address, streamOffset
            else
                address, register, streamOffset

        | Mode.Memory0D16 ->
            let register = Registers.fromId reg w |> Register
            let address =
                DirectAddress (read16bits (stream[at + streamOffset]) (stream[at + streamOffset + 1]) isWord, isWord)
            
            if sourceIsRegister d then
                register, address, streamOffset + 2
            else
                address, register, streamOffset + 2
        | Mode.MemoryD8 ->
            let register = Registers.fromId reg w |> Register
            let address =
                let registers = Registers.effectiveAddress rm
                let offset = read8bits stream[at + streamOffset] isWord
                Formula (registers, offset, isWord)
            
            if sourceIsRegister d then
                register, address, streamOffset + 1
            else
                address, register, streamOffset + 1
        | Mode.MemoryD16 ->
            let register = Registers.fromId reg w |> Register
            let address =
                let registers = Registers.effectiveAddress rm
                let offset = read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord
                Formula (registers, offset, isWord)
            
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
    
let immediateCase (stream: byte[]) at =
    let s = get7thBit stream[at]
    let w = get8thBit stream[at]
    let isWord = isWord w
    let signExtend = signExtend s
    let ``mod`` = getFirst2Bits stream[at+1]
    let _opcodeExtension = get3thTo5thBits stream[at+1]
    let rm = getLast3Bits stream[at+1]

    let memoryMode = parseMemoryMode ``mod`` rm
    let streamOffset = 2
    
    let destination, streamOffset = 
        match memoryMode with
        | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0, isWord), streamOffset
        | Mode.Memory0D16 -> DirectAddress (read16bits (stream[at + streamOffset]) (stream[at + streamOffset + 1]) isWord, isWord), streamOffset + 2
        | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[at + streamOffset] signExtend, isWord), streamOffset + 1
        | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord, isWord), streamOffset + 2
        | Mode.Register -> Register (Registers.fromId rm w), streamOffset
    
    let data, streamOffset = 
        if not signExtend && isWord then
            read16bits stream[at + streamOffset] stream[at + streamOffset + 1] signExtend, streamOffset + 2
        else
            read8bits stream[at + streamOffset] signExtend, streamOffset + 1
    
    destination, data, streamOffset
let accumulatorCase (stream : byte[]) at instructionConstructor =
    let isWord = isWord (get8thBit stream[at])
    let streamOffset = 1
    let data, streamOffset =
        if isWord then
            read16bits stream[at+streamOffset] stream[at+streamOffset+1] isWord, streamOffset + 2
        else
            read8bits stream[at+streamOffset] (not isWord), streamOffset + 1
    
    let instruction =
        if isWord then
            instructionConstructor(Register Register.AX, data)
        else
            instructionConstructor(Register Register.AL, data)
    
    instruction, streamOffset
    
let jumpSwitch (stream : byte[]) at op opNegated : Instruction =
    let n = get8thBit stream[at]
    let jump = read8bits stream[at + 1] false
    
    if n = 0b0uy then
        op jump
    else
        opNegated jump

let decodeInstruction (stream : byte[]) at =
    match stream[at] with
    | opcode when (opcode >>> 4) = MOV_IMMEDIATE_TO_REGISTER ->
        let w = get5thBit stream[at]
        let isWord = isWord w
        let reg = getLast3Bits stream[at]

        let streamOffset = 1
        let data, streamOffset =
            if isWord then
                read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord, streamOffset + 2
            else
                read8bits stream[at + streamOffset] (not isWord), streamOffset + 1
                
        Instruction.MovImmediateRegisterMemory(Register (Registers.fromId reg w), data), streamOffset
    | opcode when (opcode >>> 1) = MOVE_IMMEDIATE_TO_REGISTER_MEMORY ->
        let w = get8thBit stream[at]
        let isWord = isWord w
        let ``mod`` = getFirst2Bits stream[at+1]
        let _opcodeExtension = get3thTo5thBits stream[at+1]
        let rm = getLast3Bits stream[at+1]
        
        let memoryMode = parseMemoryMode ``mod`` rm
        
        let streamOffset = 2
        
        let destination, streamOffset = 
            match memoryMode with
            | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0, isWord), streamOffset
            | Mode.Memory0D16 -> DirectAddress (read16bits (stream[at + streamOffset]) (stream[at + streamOffset + 1]) isWord, isWord), streamOffset + 2
            | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[at + streamOffset] isWord, isWord), streamOffset + 1
            | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord, isWord), streamOffset + 2
            | Mode.Register -> Register (Registers.fromId rm w), streamOffset

        let data, streamOffset = 
            if isWord then
                read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord, streamOffset + 2
            else
                read8bits stream[at + streamOffset] (not isWord), streamOffset + 1

        Instruction.MovImmediateRegisterMemory (destination, data), streamOffset
    | opcode when (opcode >>> 2) = MOVE_TO_FROM_ACCUMULATOR ->
        let toMemory = (get7thBit stream[at]) = 0b1uy
        let w = get8thBit stream[at]
        let isWord = isWord w
        let streamOffset = 1
        let memory, streamOffset = 
            if isWord then
                DirectAddress (read16bits stream[at+streamOffset] stream[at+streamOffset+1] isWord, isWord), streamOffset + 2
            else
                DirectAddress (read8bits stream[at+streamOffset] isWord, isWord), streamOffset + 1

        let register = Register Register.AX
        let instruction =
            if toMemory then
                Instruction.Mov (memory, register)
            else
                Instruction.Mov (register, memory)
                
        instruction, streamOffset
    | opcode when (opcode >>> 2) = MOVE_REGISTER_MEMORY_TO_FROM_SEGMENT_REGISTER ->
        let d = get7thBit stream[at]
        let w = 0b1uy
        let isWord = isWord w
        
        let ``mod`` = getFirst2Bits stream[at + 1]
        let sr = get4thAnd5thBits stream[at + 1]
        let rm = getLast3Bits stream[at + 1]
        
        let memoryMode = parseMemoryMode ``mod`` rm
        
        let streamOffset = 2
        
        let address, streamOffset = 
            match memoryMode with
            | Mode.Memory0 -> Formula (Registers.effectiveAddress rm, 0, isWord), streamOffset
            | Mode.Memory0D16 -> DirectAddress (read16bits (stream[at + streamOffset]) (stream[at + streamOffset + 1]) isWord, isWord), streamOffset + 2
            | Mode.MemoryD8 -> Formula (Registers.effectiveAddress rm, read8bits stream[at + streamOffset] isWord, isWord), streamOffset + 1
            | Mode.MemoryD16 -> Formula (Registers.effectiveAddress rm, read16bits stream[at + streamOffset] stream[at + streamOffset + 1] isWord, isWord), streamOffset + 2
            | Mode.Register -> Register (Registers.fromId rm w), streamOffset
            
        let segmentRegister =
            match sr with
            | 0b00uy -> Register.ES
            | 0b01uy -> Register.CS
            | 0b10uy -> Register.SS
            | 0b11uy -> Register.DS
            | sr -> failwith $"Unknown segment register '%B{sr}'"
        
        let instruction =
            if d = 0b0uy then
                Instruction.Mov(address, Register segmentRegister)
            else
                Instruction.Mov(Register segmentRegister, address)
            
        instruction, streamOffset
    | opcode when (opcode >>> 2) = MOV ->
        let source, destination, streamOffset = baseCase stream at
        Instruction.Mov (destination,source), streamOffset
    | opcode when (opcode >>> 2) = ADD ->
        let source, destination, streamOffset = baseCase stream at
        Instruction.Add (destination,source), streamOffset
    | opcode when (opcode >>> 2) = SUB ->
        let source, destination, streamOffset = baseCase stream at            
        Instruction.Sub (destination,source), streamOffset
    | opcode when (opcode >>> 2) = CMP ->
        let source, destination, streamOffset = baseCase stream at            
        Instruction.Cmp (destination,source), streamOffset
    | opcode when (opcode >>> 2) = ADD_IMMEDIATE_TO_REGISTER_MEMORY && (get3thTo5thBits stream[at+1] = 0b000uy) ->
        let destination, data, streamOffset = immediateCase stream at
        Instruction.AddImmediateToRegisterMemory(destination, data), streamOffset
    | opcode when (opcode >>> 2) = SUB_IMMEDIATE_TO_REGISTER_MEMORY && (get3thTo5thBits stream[at+1] = 0b101uy) ->
        let destination, data, streamOffset = immediateCase stream at
        Instruction.SubImmediateToRegisterMemory(destination, data), streamOffset
    | opcode when (opcode >>> 2) = CMP_IMMEDIATE_TO_REGISTER_MEMORY && (get3thTo5thBits stream[at+1] = 0b111uy) ->
        let destination, data, streamOffset = immediateCase stream at
        Instruction.CmpImmediateToRegisterMemory(destination, data), streamOffset
    | opcode when (opcode >>> 1) = ADD_IMMEDIATE_TO_ACCUMULATOR ->
        let instruction, streamOffset = accumulatorCase stream at Instruction.AddImmediateToRegisterMemory
        instruction, streamOffset
    | opcode when (opcode >>> 1) = SUB_IMMEDIATE_TO_ACCUMULATOR ->
        let instruction, streamOffset = accumulatorCase stream at Instruction.SubImmediateToRegisterMemory
        instruction, streamOffset
    | opcode when (opcode >>> 1) = CMP_IMMEDIATE_TO_ACCUMULATOR ->
        let instruction, streamOffset = accumulatorCase stream at Instruction.CmpImmediateToRegisterMemory
        instruction, streamOffset
    | opcode when (opcode >>> 1) = JMP_EQUAL_ZERO_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jz Instruction.Jnz
        jump, 2
    | opcode when (opcode >>> 1) = JPM_ON_LESS_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jl Instruction.Jnl
        jump, 2
    | opcode when (opcode >>> 1) = JPM_ON_LESS_OR_EQUAL_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jle Instruction.Jnle
        jump, 2
    | opcode when (opcode >>> 1) = JMP_ON_BELOW_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jb Instruction.Jnb
        jump, 2
    | opcode when (opcode >>> 1) = JMP_ON_BELOW_OR_EQUAL_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jbe Instruction.Jnbe
        jump, 2
    | opcode when (opcode >>> 1) = JMP_ON_PARITY_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jp Instruction.Jnp
        jump, 2
    | opcode when (opcode >>> 1) = JMP_ON_OVERFLOW_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Jo Instruction.Jno
        jump, 2
    | opcode when (opcode >>> 1) = JMP_ON_SIGN_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Js Instruction.Jns
        jump, 2
    | opcode when (opcode >>> 1) = LOOP_CX_TIMES_OR_ON_CX_ZERO ->
        let jump = jumpSwitch stream at Instruction.Loop Instruction.Jcxz
        jump, 2
    | opcode when (opcode >>> 1) = LOOP_WHILE_ZERO_OR_NEGATION ->
        let jump = jumpSwitch stream at Instruction.Loopnz Instruction.Loopz
        jump, 2
    | opcode -> failwithf $"opcode '%06B{opcode}' not yet supported."
    
let decode (stream : byte[]) at = seq {
    let instruction, streamOffset = decodeInstruction stream at
    yield instruction, at + streamOffset
}

let decodeAll (stream : byte[]) =
    let mutable at = 0
    let instructions = ResizeArray()
    while at < stream.Length - 1 do
        let instruction, streamOffset = decodeInstruction stream at
        instructions.Add(instruction)
        at <- at + streamOffset

    instructions.ToArray()