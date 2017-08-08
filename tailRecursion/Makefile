run: program
	./program

program: rec.cmo
	ocamlc -g -o program rec.cmo

rec.cmo: rec.ml
	ocamlc -c rec.ml
