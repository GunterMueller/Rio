.section .text

	.globl unwind
	.type unwind, @function
unwind:
	xorq %rbx, %rbx
	movq (%rsp), %r10
	movb (%r10), %bl
	jmp *.unwind_table(, %ebx, 8)

notag:
	jmp .tag_error

link:
	popq %r12
	pushq 8(%r12)
	jmp unwind

app:
	pushq 8(%r10)
	jmp unwind

sc:
	movl %esp, %edx
	subl %ebp, %edx
	cmpl 16(%r10), %edx
	ja .arity_error
	jmpq *8(%r10)

.arity_error:
	movq $.arity_err, %rdi
	movl 16(%r10), %esi
	call printf
	mov $1, %rdi
	call exit

integer:
	movq $.int_format, %rdi
	movq 8(%r10), %rsi
	xor %rax, %rax
	call printf
	movq $0, %rdi
	call exit

.tag_error:
	movq $.tag_fmt, %rdi
	xor %rax, %rax
	call printf
	movq $3, %rdi
	call exit

.section .rodata

.int_format: .asciz "%d\n"
.tag_fmt: .asciz "entered a value with tag 0"
.arity_err:
	.asciz "supercombinator arity error: expected stack displacement of %d, but got %d\n"
.unwind_table: .8byte notag, link, sc, app, integer
