#Makefile for pi-calculus
SRC = syntax.mli syntax.ml simpletype.mli simpletype.ml complextype.ml execution.ml parser.mli parser.ml lexer.ml main.ml

all: clean $(SRC)
	ocamlc -w A-4 -g -o checker $(SRC)

parser.mli parser.ml:
	ocamlyacc parser.mly

lexer.ml:
	ocamllex lexer.mll

clean:
	rm -f *.cmi *.cmx *.o *.cmo parser.ml parser.mli lexer.ml
