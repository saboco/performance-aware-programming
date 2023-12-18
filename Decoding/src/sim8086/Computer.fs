module sim8086.Computer

open System.Data.Common
open Decoder

let printRegister (before : byte[]) (after : byte[]) r =
    let register, dataBefore, dataAfter =
        match r with
        | Register.AX ->
            let dataBefore = read16bits before[0] before[1] true|> uint16
            let dataAfter = read16bits after[0] after[1] true|> uint16
            "ax", dataBefore, dataAfter
        | Register.AL ->
            let dataBefore = read8bits before[0] true|> uint16
            let dataAfter = read8bits before[0] true|> uint16
            "al", dataBefore, dataAfter
        | Register.AH ->
            let dataBefore = read8bits before[1] true|> uint16
            let dataAfter = read8bits after[1] true|> uint16
            "ah", dataBefore, dataAfter
        | Register.BX ->
            let dataBefore = read16bits before[2] before[3] true|> uint16
            let dataAfter = read16bits after[2] after[3] true|> uint16
            "bx", dataBefore, dataAfter
        | Register.BL ->
            let dataBefore = read8bits before[2] true|> uint16
            let dataAfter = read8bits after[2] true|> uint16
            "bl", dataBefore, dataAfter
        | Register.BH ->
            let dataBefore = read8bits before[3] true|> uint16
            let dataAfter = read8bits after[3] true|> uint16
            "bh", dataBefore, dataAfter
        | Register.CX ->
            let dataBefore = read16bits before[4] before[5] true|> uint16
            let dataAfter = read16bits after[4] after[5] true|> uint16
            "cx", dataBefore, dataAfter
        | Register.CL -> 
            let dataBefore = read8bits before[4] true|> uint16
            let dataAfter = read8bits after[4] true|> uint16
            "cl", dataBefore, dataAfter
        | Register.CH -> 
            let dataBefore = read8bits before[5] true|> uint16
            let dataAfter = read8bits after[5] true|> uint16
            "ch", dataBefore, dataAfter
        | Register.DX -> 
            let dataBefore = read16bits before[6] before[7] true|> uint16
            let dataAfter = read16bits after[6] after[7] true|> uint16
            "dx", dataBefore, dataAfter
        | Register.DL -> 
            let dataBefore = read8bits before[6] true|> uint16
            let dataAfter = read8bits after[6] true|> uint16
            "dl", dataBefore, dataAfter
        | Register.DH -> 
            let dataBefore = read8bits before[7] true|> uint16
            let dataAfter = read8bits after[7] true|> uint16
            "dh", dataBefore, dataAfter
        | Register.SP -> 
            let dataBefore = read16bits before[8] before[9] true|> uint16
            let dataAfter = read16bits after[8] after[9] true|> uint16
            "sp", dataBefore, dataAfter
        | Register.BP -> 
            let dataBefore = read16bits before[10] before[11] true |> uint16
            let dataAfter = read16bits after[10] after[11] true|> uint16
            "bp", dataBefore, dataAfter
        | Register.SI -> 
            let dataBefore = read16bits before[12] before[13] true|> uint16
            let dataAfter = read16bits after[12] after[13] true|> uint16
            "si", dataBefore, dataAfter
        | Register.DI -> 
            let dataBefore = read16bits before[14] before[15] true|> uint16
            let dataAfter = read16bits after[14] after[15] true|> uint16
            "di", dataBefore, dataAfter
        | Register.ES -> 
            let dataBefore = read16bits before[16] before[17] true|> uint16
            let dataAfter = read16bits after[16] after[17] true|> uint16
            "es", dataBefore, dataAfter
        | Register.CS -> 
            let dataBefore = read16bits before[18] before[19] true |> uint16
            let dataAfter = read16bits after[18] after[19] true |> uint16
            "cs", dataBefore, dataAfter
        | Register.SS -> 
            let dataBefore = read16bits before[20] before[21] true|> uint16
            let dataAfter = read16bits after[20] after[21] true|> uint16
            "ss", dataBefore, dataAfter
        | Register.DS -> 
            let dataBefore = read16bits before[22] before[23] true|> uint16
            let dataAfter = read16bits after[22] after[23] true|> uint16
            "ds", dataBefore, dataAfter
        | Register.IP -> 
            let dataBefore = read16bits before[24] before[25] true|> uint16
            let dataAfter = read16bits after[24] after[25] true|> uint16
            "ip", dataBefore, dataAfter
            
    if dataBefore <> dataAfter then
        printf $"{register}=0x%04x{dataBefore}->0x%04x{dataAfter}"
    
let listOfRegisters = [|
    Register.AX
    Register.BX
    Register.CX
    Register.DX        
    Register.SP
    Register.BP
    Register.SI
    Register.DI
    Register.ES
    Register.CS
    Register.SS
    Register.DS
    Register.IP
|]

let printRegisters before after =
    for register in listOfRegisters do
        printRegister before after register
 
let memory = Array.zeroCreate<byte> 1_048_576
let registers = Array.zeroCreate<byte> 26
let flags = [|0us|]

[<RequireQualifiedAccess>]
module Flags =
    let [<Literal>] CF = 0
    let [<Literal>] PF = 2
    let [<Literal>] AF = 4
    let [<Literal>] ZF = 6
    let [<Literal>] SF = 7
    let [<Literal>] TF = 8
    let [<Literal>] IF = 9
    let [<Literal>] DF = 10
    let [<Literal>] OF = 11
    
    let isBitSet bitIndex data = data &&& (1us <<< bitIndex) <> 0us
    
    let listOfFlags = [|
        CF
        PF
        AF
        ZF
        SF
        TF
        IF
        DF
        OF
    |]
    
    let sprintFlag flag =
        match flag with
        | CF -> "C"
        | PF -> "P"
        | AF -> "A"
        | ZF -> "Z"
        | SF -> "S"
        | TF -> "T"
        | IF -> "I"
        | DF -> "D"
        | OF -> "O"
        | f -> failwithf $"Unknown flag '{f}'"
        
    let printFlags before after =
        printf "Flags: "
        for flag in listOfFlags do
            if isBitSet flag before then
                printf $"%s{sprintFlag flag}"
        
        printf "->"
        
        for flag in listOfFlags do
            if isBitSet flag after then
                printf $"%s{sprintFlag flag}"
    
    let setFlag flag = flags[0] <- flags[0] ||| (1us <<< flag)
    let unsetFlag flag = flags[0] <- flags[0] &&& ~~~(1us <<< flag)
    let isHighBitSet (data : uint16) = isBitSet 15 data
    let isHigherLowerBitSet (data : uint16) = isBitSet 7 data
    let checkParity (data : uint16) =
        let mutable count = 0
        for i in 0..7 do
            if isBitSet i data then
                count <- count + 1
        
        count % 2 = 0
    
    let rec setFlags (data : int) =
        let data = uint16 data
        if data = 0us then
            setFlag ZF
        else
            unsetFlag ZF
        
        if isHighBitSet data then
            setFlag SF
        else
            unsetFlag SF
                    
        if checkParity data then
            setFlag PF
        else
            unsetFlag PF
     
let readRegister r =
    match r with
    | Register.AX -> read16bits registers[0] registers[1] true
    | Register.AL -> read8bits registers[0] true
    | Register.AH -> read8bits registers[1] true
    | Register.BX -> read16bits registers[2] registers[3] true
    | Register.BL -> read8bits registers[2] true
    | Register.BH -> read8bits registers[3] true
    | Register.CX -> read16bits registers[4] registers[5] true
    | Register.CL -> read8bits registers[4] true
    | Register.CH -> read8bits registers[5] true
    | Register.DX -> read16bits registers[6] registers[7] true
    | Register.DL -> read8bits registers[6] true
    | Register.DH -> read8bits registers[7] true
    | Register.SP -> read16bits registers[8] registers[9] true
    | Register.BP -> read16bits registers[10] registers[11] true
    | Register.SI -> read16bits registers[12] registers[13] true
    | Register.DI -> read16bits registers[14] registers[15] true
    | Register.ES -> read16bits registers[16] registers[17] true
    | Register.CS -> read16bits registers[18] registers[19] true
    | Register.SS -> read16bits registers[20] registers[21] true
    | Register.DS -> read16bits registers[22] registers[23] true
    | Register.IP -> read16bits registers[24] registers[25] true
    
let inline write16BitsRegister idxLo idxHi data = write16Bits registers idxLo idxHi data
let inline write8BitsRegister idx data = write8bits registers idx data

let writeRegister r data =
    match r with
    | Register.AX -> write16BitsRegister 0 1 data
    | Register.AL -> write8BitsRegister 0 data
    | Register.AH -> write8BitsRegister 1 data
    | Register.BX -> write16BitsRegister 2 3 data
    | Register.BL -> write8BitsRegister 2 data
    | Register.BH -> write8BitsRegister 3 data
    | Register.CX -> write16BitsRegister 4 5 data
    | Register.CL -> write8BitsRegister 4 data
    | Register.CH -> write8BitsRegister 5 data
    | Register.DX -> write16BitsRegister 6 7 data
    | Register.DL -> write8BitsRegister 6 data
    | Register.DH -> write8BitsRegister 7 data
    | Register.SP -> write16BitsRegister 8 9 data 
    | Register.BP -> write16BitsRegister 10 11 data
    | Register.SI -> write16BitsRegister 12 13 data
    | Register.DI -> write16BitsRegister 14 15 data
    | Register.ES -> write16BitsRegister 16 17 data
    | Register.CS -> write16BitsRegister 18 19 data
    | Register.SS -> write16BitsRegister 20 21 data
    | Register.DS -> write16BitsRegister 22 23 data
    | Register.IP -> write16BitsRegister 24 25 data
    
let memoryAddress (registers : Register[]) offset =
    let mutable addr = 0
    for register in registers do
        addr <- addr + readRegister register
           
    addr + offset
    
let readMemory isWord idx =
    if isWord then
        read16bits memory[idx] memory[idx + 1] true 
    else
        read8bits memory[idx] isWord

let inline write8bitsMemory idx data = write8bits memory idx data
let inline write16bitsMemory idx data = write16Bits memory idx (idx + 1) data

let writeMemory isWord idx data =        
    if isWord then
        write16bitsMemory idx data
    else
        write8bitsMemory idx data
            
let read (address : Address) =
    match address with
    | Register r -> readRegister r
    | DirectAddress (idx, isWord) -> readMemory isWord idx
    | Formula (registers, offset, isWord) -> (memoryAddress registers offset) |> readMemory isWord
        
let write (at : Address) data =
    match at with
    | Register r -> writeRegister r data
    | DirectAddress (idx, isWord) -> writeMemory isWord idx data
    | Formula (registers, offset, isWord) ->
        let idx = memoryAddress registers offset
        writeMemory isWord idx data

let executeInstruction (instruction : Instruction) at = seq {
    let at = 
        match instruction with
        | Instruction.Mov (dst, src) ->
            read src |> write dst
            at
        | Instruction.Add (dst, src) ->
            let a = read src
            let b = read dst
            let result = a + b
            Flags.setFlags result
            write dst result
            at
        | Instruction.Sub (dst, src) ->
            let a = read src
            let b = read dst
            let result = a - b
            Flags.setFlags result
            write dst result
            at
        | Instruction.Cmp (dst, src) ->
            let a = read src 
            let b = read dst
            Flags.setFlags (a - b)
            at
        | Instruction.MovImmediateRegisterMemory (dst, data) ->
            write dst data
            at
        | Instruction.AddImmediateToRegisterMemory (dst, data) ->
            let b = read dst
            let result = b + data
            Flags.setFlags result
            write dst result
            at
        | Instruction.SubImmediateToRegisterMemory (dst, data) ->
            let b = read dst
            let result = b - data
            Flags.setFlags result
            write dst result
            at
        | Instruction.CmpImmediateToRegisterMemory (dst, data) ->
            let b = read dst
            Flags.setFlags (b - data)
            at
        | Instruction.Jz v -> failwithf "not implemented"
        | Instruction.Jnz v -> failwithf "not implemented"
        | Instruction.Jl v -> failwithf "not implemented"
        | Instruction.Jnl v -> failwithf "not implemented"
        | Instruction.Jle v -> failwithf "not implemented"
        | Instruction.Jnle v -> failwithf "not implemented"
        | Instruction.Jb v -> failwithf "not implemented"
        | Instruction.Jnb v -> failwithf "not implemented"
        | Instruction.Jbe v -> failwithf "not implemented"
        | Instruction.Jnbe v -> failwithf "not implemented"
        | Instruction.Jp v -> failwithf "not implemented"
        | Instruction.Jnp v -> failwithf "not implemented"
        | Instruction.Jo v -> failwithf "not implemented"
        | Instruction.Jno v -> failwithf "not implemented"
        | Instruction.Js v -> failwithf "not implemented"
        | Instruction.Jns v -> failwithf "not implemented"
        | Instruction.Loop v -> failwithf "not implemented"
        | Instruction.Loopz v -> failwithf "not implemented"
        | Instruction.Loopnz v -> failwithf "not implemented"
        | Instruction.Jcxz v -> failwithf "not implemented"
    
    yield at
}