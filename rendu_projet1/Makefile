all: rapport calcul_exp
	make rapport
	make calcul_exp
	

calcul_exp: x86_64.mli x86_64.ml proj_prog_final_ocaml.ml
	ocamlc x86_64.mli x86_64.ml proj_prog_final_ocaml.ml -o aritha


rapport: rapport.tex
	pdflatex -shell-escape rapport.tex

clean:
	rm -rf x86_64.cmi *.cmo
	rm -rf proj_prog_final_ocaml.cmi *.cmo
	rm -rf rapport.log *.aux
