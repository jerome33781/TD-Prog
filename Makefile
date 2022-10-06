all: test_list.ml
	ocamlc -o test_list test_list.ml

clean:
	rm -rf test_list *.cmi *.cmo *~
