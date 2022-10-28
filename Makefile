all: rapport aff_res
	make rapport
	make aff_res
	

calcul_exp: x86_64.mli x86_64.ml proj_prog_final_ocaml.ml expression.exp
	ocamlc x86_64.mli x86_64.ml proj_prog_final_ocaml.ml -o aritha

calc_exp: calcul_exp aritha expression.exp
	./aritha expression.exp

cal_asm: calc_exp resultat.s
	gcc -no-pie -o res resultat.s

aff_res: cal_asm res
	./res

rapport: rapport.tex
	pdflatex -shell-escape rapport.tex

clean:
	rm -rf x86_64.cmi *.cmo
	rm -rf proj_prog_final_ocaml.cmi *.cmo
	rm res
	rm -rf rapport.log *.aux
