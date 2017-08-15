run: program
	./program

program: ocaml99.cmo
	ocamlc -g -o program ocaml99.cmo

ocaml99.cmo: ocaml99.ml
	ocamlc -c ocaml99.ml
