all: test_my_list rapport.pdf

	make test_my_list
	make rapport.pdf

test_my_list: my_list.mli my_list.ml test_list.ml
	ocamlc my_list.mli my_list.ml test_list.ml -o test_my_list


rapport.pdf: rapport.tex
	pdflatex -shell-escape rapport.tex

clean:
	rm -rf rapport.pdf *.aux *.pyg *.out *.log  test_my_list *.cmi *.cmo *~
	rm -r _minted-rapport
