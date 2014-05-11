#Makefile for pi-calculus
SRC = syntax.mli syntax.ml parser.mli parser.ml lexer.ml main.ml

all: $(SRC)
	ocamlopt -o checker unix.cmxa $(SRC)

parser.mli parser.ml:
	ocamlyacc parser.mly

lexer.ml:
	ocamllex lexer.mll


.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	ocamlc $(SRC)

clean:
	rm *.cmi *.cmx *.o *.cmo parser.ml parser.mli lexer.ml