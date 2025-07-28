	.text
	.globl	_alloca
	.p2align	2
_alloca:
	// ARM64版本的alloca实现
	// 输入：x0 = 需要分配的字节数
	// 输出：x0 = 分配的地址
	// 保存链接寄存器
	add x0, x0, #15
	and x0, x0, #0xfffffffffffffff0
	sub	sp, sp, x0
	// 返回分配的地址
	add x0, sp,16
	// 恢复链接寄存器
	ret