.PHONY: build
build:
	ocamlc -c structure.ml

	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli

	ocamlc -c \
		lexer.ml \
		parser.ml \
		printing.ml \
		evaluation.ml \
		cli.ml

	ocamlc -o kalami \
		lexer.cmo \
		parser.cmo \
		structure.cmo \
		printing.cmo \
		evaluation.cmo \
		cli.cmo

.PHONY: clean
clean:
	rm *.cmi
	rm *.cmo
	rm lexer.ml
	rm parser.ml
	rm parser.mli
