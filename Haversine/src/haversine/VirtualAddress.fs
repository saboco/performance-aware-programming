module haversine.VirtualAddress
open System

[<Struct>]
type VirtualAddress = {
    PML4Index : UInt16
    DirectoryPtrIndex : UInt16
    DirectoryIndex : UInt16 option
    TableIndex : UInt16 option
    Offset : UInt32
}

let print ({
    PML4Index =pML4Index
    DirectoryPtrIndex = directoryPtrIndex
    DirectoryIndex = directoryIndexO 
    TableIndex =tableIndexO
    Offset = offset }) =
    
    let directoryIndex =
        match directoryIndexO with
        | Some v -> v
        | None -> 0us
    let tableIndex =
        match tableIndexO with
        | Some v -> v
        | None -> 0us
        
    printf $"|%9i{pML4Index}|%9i{directoryPtrIndex}|%9i{directoryIndex}|%9i{tableIndex}|%12i{offset}|"
    
let printAsLine label vAddr =
    printf $"%16s{label}"
    print vAddr
    printfn ""
    
let decomposePointer4K (ptr : nativeint) : VirtualAddress =
    let address = uint64 ptr
     
    {   PML4Index = uint16 ((address >>> 39) &&& 0x1ffUL)
        DirectoryPtrIndex = uint16 ((address >>> 30) &&& 0x1ffUL)
        DirectoryIndex = Some(uint16 ((address >>> 21) &&& 0x1ffUL)) 
        TableIndex =Some (uint16 ((address >>> 12) &&& 0x1ffUL))
        Offset = uint32 ((address >>> 0) &&& 0xfffUL) }
    
let decomposePointer2MB (ptr : nativeint) : VirtualAddress =
    let address = uint64 ptr
     
    {   PML4Index = uint16 ((address >>> 39) &&& 0x1ffUL)
        DirectoryPtrIndex = uint16 ((address >>> 30) &&& 0x1ffUL)
        DirectoryIndex = Some (uint16 ((address >>> 21) &&& 0x1ffUL))
        TableIndex = None 
        Offset = uint32 ((address >>> 0) &&& 0x1fffffUL) }

let decomposePointer1GB (ptr : nativeint) : VirtualAddress =
    let address = uint64 ptr
     
    {   PML4Index = uint16 ((address >>> 39) &&& 0x1ffUL)
        DirectoryPtrIndex = uint16 ((address >>> 30) &&& 0x1ffUL)
        DirectoryIndex = None
        TableIndex = None 
        Offset = uint32 ((address >>> 0) &&& 0x3fffffffUL) }