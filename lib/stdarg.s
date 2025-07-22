	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_va_init
	.p2align	2
_va_init:
	sub	sp, sp, #224
	stp	x29, x30, [sp, #208]
	add	x29, sp, #208
	stur	x0, [x29, #-4]
	mov	x10, x0
	mov	x0, #1
	mov	x11, x0
	mov	x0, #8
	mov	x2, x0
	mov	x1, x11
	mul	x0, x1, x2
	mov	x2, x0
	mov	x1, x10
	add	x0, x1, x2
	b	.Lva_init_epilogue
.Lva_init_epilogue:
	ldp	x29, x30, [sp, #208]
	add	sp, sp, #224
	ret
	.globl	_va_next
	.p2align	2
_va_next:
	sub	sp, sp, #240
	stp	x29, x30, [sp, #224]
	add	x29, sp, #224
	stur	x0, [x29, #-4]
	mov	x1, x0
	ldr	x0, [x1]
	mov	x1, x0
	ldr	x0, [x1]
	mov	x1, x0
	str	x1, [sp, #224]
	ldr	x1, [sp, #224]
	str	x1, [sp, #16]
	mov	x1, x0
	str	x1, [sp, #224]
	ldr	x1, [sp, #224]
	str	x1, [sp, #8]
	ldr	x0, [sp, #8]
	mov	x1, x0
	ldr	x0, [x1]
	mov	x10, x0
	mov	x0, #1
	mov	x11, x0
	mov	x0, #8
	mov	x2, x0
	mov	x1, x11
	mul	x0, x1, x2
	mov	x2, x0
	mov	x1, x10
	add	x0, x1, x2
	mov	x1, x0
	str	x1, [sp, #224]
	ldr	x0, [sp, #8]
	ldr	x1, [sp, #224]
	str	x1, [x0]
	ldr	x0, [sp, #16]
	b	.Lva_next_epilogue
.Lva_next_epilogue:
	ldp	x29, x30, [sp, #224]
	add	sp, sp, #240
	ret
