global  Read_Chunk_x4
export Read_Chunk_x4
global  Read_Chunk_x8
export Read_Chunk_x8
global Read_Chunk_x8_DoubleLoop
export Read_Chunk_x8_DoubleLoop

section .text

Read_Chunk_x4:
    xor r10, r10
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
    jnz .loop
    ret

Read_Chunk_x8:
    xor r10, r10
    mov rax, rdx
	align 64
.loop:
    vmovdqu ymm0, [rax]
    vmovdqu ymm0, [rax + 32]
    vmovdqu ymm0, [rax + 64]
    vmovdqu ymm0, [rax + 96]
    vmovdqu ymm0, [rax + 128]
    vmovdqu ymm0, [rax + 160]
    vmovdqu ymm0, [rax + 192]
    vmovdqu ymm0, [rax + 224]
    add r10, 256
    and r10, r8 
    mov rax, r10
    add rax, rdx
    sub rcx, 256
    jnz .loop
    ret

Read_Chunk_x8_DoubleLoop:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
   .innerloop:
	vmovdqu ymm0, [rax]
	vmovdqu ymm0, [rax + 32]
	vmovdqu ymm0, [rax + 64]
	vmovdqu ymm0, [rax + 96]
	vmovdqu ymm0, [rax + 128]
	vmovdqu ymm0, [rax + 160]
	vmovdqu ymm0, [rax + 192]
	vmovdqu ymm0, [rax + 224]
	add rax, 256
	sub r10, 256
	jnz .innerloop
    sub rcx, r8
    jnz .loop
    ret
