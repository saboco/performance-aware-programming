bits 16

add bx, [bx + si]
add bx, [bp]
add ax, byte 33538
add ax, byte 33538
add ax, byte 776
add bx, [bp]
add cx, [bx + 2]
add bh, [bp + si + 4]
add di, [bp + di + 6]
add [bx + si], bx
add [bp], bx
add [bp], bx
add [bx + 2], cx
add [bp + si + 4], bh
add [bp + di + 6], di
add [bx], byte 130
add [bp + si + 1000], byte 797
add ax, [bp]
add al, [bx + si]
add ax, bx
add al, ah
add ax, 1000
add ax, 226
add ax, 9
sub bx, [bx + si]
sub bx, [bp]
sub bp, byte 33538
sub bp, byte 33538
sub bp, byte 11016
sub bx, [bp]
sub cx, [bx + 2]
sub bh, [bp + si + 4]
sub di, [bp + di + 6]
sub [bx + si], bx
sub [bp], bx
sub [bp], bx
sub [bx + 2], cx
sub [bp + si + 4], bh
sub [bp + di + 6], di
sub [bx], byte 41
sub [bx + di], byte 70
sub ax, [bp]
sub al, [bx + si]
sub ax, bx
sub al, ah
sub ax, 1000
sub ax, 226
sub ax, 9
cmp bx, [bx + si]
cmp bx, [bp]
cmp di, byte 33538
cmp di, byte 33538
cmp di, byte 15112
cmp bx, [bp]
cmp cx, [bx + 2]
cmp bh, [bp + si + 4]
cmp di, [bp + di + 6]
cmp [bx + si], bx
cmp [bp], bx
cmp [bp], bx
cmp [bx + 2], cx
cmp [bp + si + 4], bh
cmp [bp + di + 6], di
cmp [bx], byte 62
cmp [4834], byte 15133
cmp ax, [bp]
cmp al, [bx + si]
cmp ax, bx
cmp al, ah
cmp ax, 1000
cmp ax, 226
cmp ax, 9
jnz 2
jnz -4
jnz -6
jnz -4
jz -2
jl -4
jle -6
jb -8
jz -10
jp -12
jb -14
jp -16
jnz -18
jnl -20
jnle -22
jnb -24
jnz -26
jnp -28
jnb -30
jnp -32
loop -34
loopz -36
loopnz -38
jcxz -40
