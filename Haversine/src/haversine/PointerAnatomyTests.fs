module haversine.PointerAnatomyTests
open Binary
open VirtualAddress

let run () =
    for _ in 0..16 do
        let  pointer : nativeint =
            Memory.Native.VirtualAlloc(
                0,
                uint64 (1024*1024),
                Memory.AllocationType.MEM_RESERVE ||| Memory.AllocationType.MEM_COMMIT,
                Memory.MemoryProtection.PAGE_READWRITE
            )
        
        let address = uint64 pointer
        printf("|");
        printBinaryBits address 48u 16u
        printf("|");
        printBinaryBits address 39u 9u
        printf("|");
        printBinaryBits address 30u 9u
        printf("|");
        printBinaryBits address 21u 9u
        printf("|");
        printBinaryBits address 12u 9u
        printf("|");
        printBinaryBits address 0u 12u
        printf("|\n");
        
        printf("|");
        printAsLine " 4k paging: " (decomposePointer4K pointer)
        printf("|");
        printAsLine "2mb paging: " (decomposePointer2MB pointer)
        printf("|");
        printAsLine "1gb paging: " (decomposePointer1GB pointer)
        
        printf("\n");