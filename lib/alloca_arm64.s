        .text
        .globl  _alloca
        .p2align        2
_alloca:
        mov     x1, x0
        mov     x0, sp
        add     x1, x1, #15
        and     x1, x1, #0xfffffffffffffff0
        sub     sp, sp, x1
        ret 