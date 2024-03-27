global Read_Aligned
export Read_Aligned
global Read_Unaligned_1
export Read_Unaligned_1
global Read_Unaligned_2
export Read_Unaligned_2
global Read_Unaligned_7
export Read_Unaligned_7
global Read_Unaligned_8
export Read_Unaligned_8
global Read_Unaligned_15
export Read_Unaligned_15
global Read_Unaligned_16
export Read_Unaligned_16

section .text

Read_Aligned:
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

Read_Unaligned_1:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
    add rax, 1
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

Read_Unaligned_2:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
    add rax, 2
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

Read_Unaligned_7:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
    add rax, 7
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

Read_Unaligned_8:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
    add rax, 8
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

Read_Unaligned_15:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
    add rax, 15
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
Read_Unaligned_16:
	align 64
.loop:
    mov r10, r8
    mov rax, rdx
    add rax, 16
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
