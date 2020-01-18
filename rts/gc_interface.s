.section .text

	.globl collect_garbage
	.type collect_garbage, @function
collect_garbage:
	xor %rax, %rax
	mov %r14, %rsi
	sub %r15, %rsi
	call gcError
	jmp unwind

.section .rodata
.overcommit_msg:
	.asciz "attempted to allocate %d bytes of memory you do not have.\n"
