.section .text

	.globl main
	.type main, @function
main:
	call gcEntry
	push %rbp
	movq %rsp, %rbp
	pushq $_main
	jmp unwind
