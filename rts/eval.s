	.globl eval
	.type eval, @function
eval:
	popq 8(%r12) # return address
	popq %rbx
	movq %rsp, (%r12) # save stack pointer

	call newStack
	movq %rax, 16(%r12) # save new stack pointer
	movq %rax, %rsp

	subq $64, %r12
	push %rbx
	jmp unwind
