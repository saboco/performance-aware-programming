﻿[<RequireQualifiedAccess>]
module sim8086.Computer

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
            
    dataBefore, dataAfter
        
    
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
        let dataBefore, dataAfter = printRegister before after register
        if dataBefore <> dataAfter then
            printf $"{register}=0x%04x{dataBefore}->0x%04x{dataAfter};"
            
let printRegistersN before after =
    for register in listOfRegisters do
        let dataBefore, dataAfter = printRegister before after register
        if dataBefore <> dataAfter then
            printfn $"{register}=0x%04x{dataBefore}->0x%04x{dataAfter} (%i{dataAfter})"
 
let mutable memory = Array.zeroCreate<byte> 1_048_576
let mutable registers = Array.zeroCreate<byte> 26
let mutable flags = [|0us|]

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
    
    let isSet flag = isBitSet flag flags[0]
    let isZero () = isSet ZF
    let isNegative () = isSet SF
    let isParity () = isSet PF
    let isOverflow () = isSet OF
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

let updateIpRegister at =
    writeRegister Register.IP at
    
let incrementIpRegister inc =
    let v = readRegister Register.IP
    writeRegister Register.IP (v + inc)
    
let decrementCx () =
    let cx = readRegister Register.CX
    let result = (cx - 1)
    Flags.setFlags result
    writeRegister Register.CX result

let executeInstruction (instruction : Instruction) at = seq {
    let at = 
        match instruction with
        | Instruction.Mov (dst, src) ->
            read src |> write dst
            at
        | Instruction.Add (dst, src) ->
            let right = read src
            let left = read dst
            let result = right + left
            Flags.setFlags result
            write dst result
            at
        | Instruction.Sub (dst, src) ->
            let right = read src
            let left = read dst
            let result = left - right
            Flags.setFlags result
            write dst result
            at
        | Instruction.Cmp (dst, src) ->
            let right = read src 
            let left = read dst
            Flags.setFlags (left - right)
            at
        | Instruction.MovImmediateRegisterMemory (dst, data) ->
            write dst data
            at
        | Instruction.AddImmediateToRegisterMemory (dst, data) ->
            let left = read dst
            let result = left + data
            Flags.setFlags result
            write dst result
            at
        | Instruction.SubImmediateToRegisterMemory (dst, data) ->
            let left = read dst
            let result = left - data
            Flags.setFlags result
            write dst result
            at
        | Instruction.CmpImmediateToRegisterMemory (dst, data) ->
            let left = read dst
            Flags.setFlags (left - data)
            at
        | Instruction.Jz jump ->
            if Flags.isZero () then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jnz jump ->
            if Flags.isZero() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jl jump ->
            if Flags.isNegative() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jnl jump ->
            if Flags.isNegative() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jle jump ->
            if Flags.isZero() || Flags.isNegative() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jnle jump ->
            if Flags.isZero() || Flags.isNegative() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jb jump ->
            if Flags.isNegative() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jnb jump ->
            if Flags.isNegative() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jbe jump ->
            if Flags.isZero() || Flags.isNegative() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jnbe jump ->
            if Flags.isZero() || Flags.isNegative() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jp jump ->
            if Flags.isParity() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jnp jump ->
            if Flags.isParity () then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jo jump ->
            if Flags.isOverflow () then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jno jump ->
            if Flags.isOverflow() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Js jump ->
            if Flags.isNegative() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Jns jump ->
            if Flags.isNegative() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Loop jump ->
            decrementCx ()
            if Flags.isZero() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Loopz jump ->
            decrementCx ()
            if Flags.isZero() then
                incrementIpRegister jump
                at + jump
            else
                at
        | Instruction.Loopnz jump ->
            decrementCx ()
            if Flags.isZero() then
                at
            else
                incrementIpRegister jump
                at + jump
        | Instruction.Jcxz jump ->
            decrementCx ()
            if Flags.isZero() then
                incrementIpRegister jump
                at + jump
            else
                at
    
    yield at
}

[<RequireQualifiedAccess>]
module Timing =
    
    let(|BaseOrIndexOnly|BpPlusDiOrBxPlusSi|BpPlusSiOrBxPlusDi|) (regs : Register[]) =
        if regs.Length = 1 then
            BaseOrIndexOnly
        else
            match regs with
            | [|Register.BP;Register.DI|] | [|Register.BX;Register.SI|] -> BpPlusDiOrBxPlusSi
            | [|Register.BP;Register.SI|] | [|Register.BX;Register.DI|] -> BpPlusSiOrBxPlusDi
            | f -> failwithf $"Invalid formula %A{f}"
            
    let effectiveAddressCalulation (src : Address) =
        match src with
        | Register _ -> 2 
        | DirectAddress _ -> 6
        | Formula (regs,displacement,isWord) ->
            match regs, displacement with
            | BaseOrIndexOnly, 0 -> 5
            | BaseOrIndexOnly, _ -> 9
            | BpPlusDiOrBxPlusSi, 0 -> 7
            | BpPlusSiOrBxPlusDi, 0 -> 8
            | BpPlusDiOrBxPlusSi, _ -> 11
            | BpPlusSiOrBxPlusDi, _ -> 12
        
    let isWordRegister (reg : Register) =
        match reg with
        | Register.AL
        | Register.AH
        | Register.BL
        | Register.BH
        | Register.CL
        | Register.CH
        | Register.DL
        | Register.DH -> false
        | Register.AX  
        | Register.CX  
        | Register.DX  
        | Register.BX  
        | Register.SP  
        | Register.BP  
        | Register.SI  
        | Register.DI  
        | Register.ES  
        | Register.CS 
        | Register.SS  
        | Register.DS 
        | Register.IP -> true
        
    let isWordOperationWithOddAddress (addr : Address) =
        match addr with
        | Register reg -> false
        | DirectAddress (addr, isWord) -> isWord && addr % 2 <> 0
        | Formula(_, addr, isWord) -> isWord && addr % 2 <> 0 
        
    let fourIfWordOperationWithOddAddress (src: Address) (dst: Address) =
        if isWordOperationWithOddAddress src || isWordOperationWithOddAddress dst then
            4
        else
            0
            
    let fourIfIsWordOperationWithOddAddress (addr : Address) =
        if isWordOperationWithOddAddress addr then
            4
        else
            0
    
    let cyclesPer (instruction : Instruction) =
        let cycles = 
            match instruction with
            | Instruction.Mov (dst, src) ->
                let baseCycles = 
                    match dst,src with
                    | Register _, Register _ ->  2
                    | Register Register.AX, DirectAddress _ | Register Register.AX, Formula _ -> 10
                    | DirectAddress _, Register Register.AX  | Formula _, Register Register.AX -> 10
                    | Register _, DirectAddress _ | Register _, Formula _ -> 8 + effectiveAddressCalulation src
                    | DirectAddress _, Register _  | Formula _, Register _ -> 9 + effectiveAddressCalulation dst
                    | dst, src -> failwithf $"Invalid combinations ADD {dst}, {src}"
                    
                baseCycles + fourIfWordOperationWithOddAddress src dst
                
            | Instruction.Add (dst, src) ->
                let baseCycles = 
                    match dst, src with
                    | Register _, Register _ ->  3
                    | Register _, DirectAddress _ | Register _, Formula _ -> 9 + effectiveAddressCalulation src
                    | DirectAddress _, Register _  | Formula _, Register _ -> 16 + effectiveAddressCalulation dst
                    | dst, src -> failwithf $"Invalid combinations ADD {dst}, {src}"
                    
                baseCycles + fourIfWordOperationWithOddAddress src dst
                
            | Instruction.Sub (dst, src) ->
                let baseCycles = 
                    match dst, src with
                    | Register _, Register _ ->  3
                    | Register _, DirectAddress _ | Register _, Formula _ -> 9 + effectiveAddressCalulation src
                    | DirectAddress _, Register _  | Formula _, Register _ -> 16 + effectiveAddressCalulation dst
                    | dst, src -> failwithf $"Invalid combinations ADD {dst}, {src}"
                    
                baseCycles + fourIfWordOperationWithOddAddress src dst
            | Instruction.Cmp (dst, src) ->
                let baseCycles = 
                    match dst, src with
                    | Register _, Register _ ->  3
                    | Register _, DirectAddress _ | Register _, Formula _ -> 9 + effectiveAddressCalulation src
                    | DirectAddress _, Register _  | Formula _, Register _ -> 9 + effectiveAddressCalulation dst
                    | dst, src -> failwithf $"Invalid combinations ADD {dst}, {src}"
                    
                baseCycles + fourIfWordOperationWithOddAddress src dst
            | Instruction.MovImmediateRegisterMemory (dst, _) ->
                let baseCycles = 
                    match dst with
                    | Register _ -> 4
                    | DirectAddress _ | Formula _ -> 10 + effectiveAddressCalulation dst
                
                baseCycles + fourIfIsWordOperationWithOddAddress dst
            | Instruction.AddImmediateToRegisterMemory (dst, _) ->
                let baseCycles = 
                    match dst with
                    | Register _ -> 4
                    | DirectAddress _ | Formula _ -> 17 + effectiveAddressCalulation dst
                
                baseCycles + fourIfIsWordOperationWithOddAddress dst
                
            | Instruction.SubImmediateToRegisterMemory (dst, _) ->
                let baseCycles = 
                    match dst with
                    | Register _ -> 4
                    | DirectAddress _ | Formula _ -> 17 + effectiveAddressCalulation dst
                
                baseCycles + fourIfIsWordOperationWithOddAddress dst
            | Instruction.CmpImmediateToRegisterMemory (dst, _) ->
                let baseCycles = 
                    match dst with
                    | Register _ -> 4
                    | DirectAddress _ | Formula _ -> 10 + effectiveAddressCalulation dst
                
                baseCycles + fourIfIsWordOperationWithOddAddress dst
            | Instruction.Jz _ -> 4
            | Instruction.Jnz _ -> 4
            | Instruction.Jl _ -> 4
            | Instruction.Jnl _ -> 4
            | Instruction.Jle _ -> 4
            | Instruction.Jnle _ -> 4
            | Instruction.Jb _ -> 4
            | Instruction.Jnb _ -> 4
            | Instruction.Jbe _ -> 4
            | Instruction.Jnbe _ -> 4
            | Instruction.Jp _ -> 4
            | Instruction.Jnp _ -> 4
            | Instruction.Jo _ -> 4
            | Instruction.Jno _ -> 4
            | Instruction.Js _ -> 4
            | Instruction.Jns _ -> 4
            | Instruction.Loop _ -> 5
            | Instruction.Loopz _ -> 6
            | Instruction.Loopnz _ -> 6
            | Instruction.Jcxz _ -> 6
            
        cycles

    type Timer() =
        let mutable operations = 0
        let mutable cycles = 0
        member _.Count(instruction : Instruction) =
            let cyclesToAdd =  cyclesPer instruction
            cycles <- cycles + cyclesToAdd
            operations <- operations + 1
            
            cyclesToAdd
            
        member _.GetCounters () = (operations, cycles, operations/cycles)
    
let executeProgram (timer : Timing.Timer) stream at = seq {
    for instruction, at in Decoder.decode stream at do
        let oldRegisters = Array.copy registers
        let oldFlags = flags[0]
        updateIpRegister at
        yield! executeInstruction instruction at
        let cycles = timer.Count(instruction)
        printf $"{Registers.sprintInstruction instruction}\t"
        printRegisters oldRegisters registers
        printf ";\t"
        Flags.printFlags oldFlags flags[0]
        printf $";\tcycles {cycles}"
        printfn ""
        
}
