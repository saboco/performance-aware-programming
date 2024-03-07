global  Read_Chunk
export Read_Chunk

section .text

Read_Chunk:
    mov rax, rdx
	align 64
.loop:
    vmovdqu ymm0, [rax]
    vmovdqu ymm0, [rax + 32]
    vmovdqu ymm0, [rax + 64]
    vmovdqu ymm0, [rax + 96]
    add r10, 128
    and r10, r8 
    mov rax, r10
    add rax, rdx
    sub rcx, 128
    jnle .loop
    ret