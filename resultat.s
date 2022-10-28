	.text
	.globl	main
main:
	movq $5, %rdi
	pushq %rdi
	movq $6, %rdi
	pushq %rdi
	movq $86, %rdi
	movq %rdi, %rax
	popq %rbx
	xorq %rdx, %rdx
	idivq %rbx
	movq %rax, %rdi
	popq %rsi
	addq %rsi, %rdi
	call print_int
	ret

print_int : 
	movq %rdi, %rsi	
	movq $S_int, %rdi
	movq $0, %rax
	call printf
	ret 

	.data
S_int:
	.string "%d\n"
