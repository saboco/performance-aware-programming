
global ReadStrided_32x2

export ReadStrided_32x2

section .text

ReadStrided_32x2:
    align 64

.outer_loop:
    mov r10, r8
    mov rax, rdx
    
.inner_loop:
    vmovdqu ymm0, [rax]
    vmovdqu ymm0, [rax + 0x20]
    add rax, r9
    dec r10
    jnz .inner_loop
    
    dec rcx
    jnz .outer_loop
    ret