	.text
	.globl	_va_init
	.p2align	2
_va_init:
	// ARM64版本的va_init实现
	// 输入：x0 = 最后一个固定参数的地址（在ARM64上，这是栈指针的值）
	// 输出：x0 = 指向第一个变长参数的指针
	
	// 在ARM64上，va_init应该返回当前栈指针的值
	// 因为变长参数从栈指针开始
	add x0, x29,16
	ret