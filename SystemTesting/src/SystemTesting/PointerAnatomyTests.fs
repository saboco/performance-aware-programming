module SystemTesting.PointerAnatomyTests
open Binary
open VirtualAddress
open Windows.Native

let run () =
    for _ in 0..16 do
        let  pointer : nativeint =
            VirtualAlloc(
                0,
                uint64 (1024*1024),
                AllocationType.MEM_RESERVE ||| AllocationType.MEM_COMMIT,
                MemoryProtection.PAGE_READWRITE
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