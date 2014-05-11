#Makefile for pi-calculus
SRC = syntax.mli syntax.ml parser.mli parser.ml lexer.ml main.ml

all: $(SRC)
	ocamlfind ocamlopt -linkpkg -package batteries -o checker $(SRC)

parser.mli parser.ml:
	ocamlyacc parser.mly

lexer.ml:
	ocamllex lexer.mll


.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	ocamlfind ocamlc -linkpkg -package batteries $(SRC)

clean:
	rm *.cmi *.cmx *.o *.cmo parser.ml parser.mli lexer.ml