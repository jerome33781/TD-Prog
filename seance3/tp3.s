
	.text
	.globl main

main : 
	subq $8, %rsp
	call quest3
	movq $0, %rax
	ret



quest1 : 
	movq $111, %rax
	ret


quest2 : 
	movq $54, %r13
	movq $57, %r14
	addq %r13, %r14
	movq %r14, %rax
	ret 

quest3 : 

	movsd val1, %xmm0
	addsd val2, %xmm0
	call print_double
	ret

print_double : 
	mov $message, %rdi
	mov $1, %rax
	call printf
	ret

	.data

val1 : .double 0.2
val2 : .double 0.22


message : 
	.string "%f\n"



