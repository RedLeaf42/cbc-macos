	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_va_init
	.p2align	2
_va_init:
	stp	x29, x30, [sp, #-16]!
	mov	x29, sp
	str	x0, [sp, #0]
	mov	x0, #8
	str	x0, [sp, #-16]!
	mov	w0, #1
	sxtw	x0, w0
	ldr	x13, [sp], #16
	mul	x0, x0, x13
	str	x0, [sp, #-16]!
	ldr	x0, [sp, #0]
	ldr	x13, [sp], #16
	add	x0, x0, x13
	b	.Lva_init_epilogue
.Lva_init_epilogue:
	ldp	x29, x30, [sp], #16
	ret
	.globl	_va_next
	.p2align	2
_va_next:
	stp	x29, x30, [sp, #-16]!
	mov	x29, sp
	sub	sp, sp, #16
	str	x0, [sp, #0]
	ldr	x0, [sp, #0]
	mov	x11, x0
	ldr	x11, [x11]
	ldr	x0, [x11]
	mov	x9, x0
	str	x9, [x29, #-8]
	ldr	x0, [sp, #0]
	mov	x9, x0
	str	x9, [x29, #-16]
	mov	x0, #8
	str	x0, [sp, #-16]!
	mov	x0, #1
	ldr	x13, [sp], #16
	mul	x0, x0, x13
	str	x0, [sp, #-16]!
	ldr	x0, [x29, #-16]
	mov	x11, x0
	ldr	x0, [x11]
	ldr	x13, [sp], #16
	add	x0, x0, x13
	mov	x9, x0
	ldr	x0, [x29, #-16]
	mov	x11, x0
	str	x9, [x11]
	ldr	x0, [x29, #-8]
	b	.Lva_next_epilogue
.Lva_next_epilogue:
	add	sp, sp, #16
	ldp	x29, x30, [sp], #16
	ret
