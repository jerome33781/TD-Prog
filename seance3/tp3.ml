

let compile x y =


	print_string ("\n \t .text \n \t .global main \n \n
main: \n
\t movq $"^(string_of_int x)^ ",%r13 \n  
\t movq $"^(string_of_int y)^ ",%r14 \n 
\t addq %r13, %r14  \n
\t movq %r14, %rax \n
\t ret \n \n ")

let _ = compile (int_of_string (Sys.argv.(1))) (int_of_string (Sys.argv.(2)))
