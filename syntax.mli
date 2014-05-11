(* syntax.mli *)

type var = string
type pos = int * int
type op = string

type valexp = Unit
	| Var of var
	| Bool of bool
	| Int of int
	| Op of op * valexp
	(*| Pair of valexp * valexp;;*)

type procexp = Zero
	| Par of procexp * procexp
	| Rep of procexp
	| Nu of var * procexp
	| If of valexp * procexp * procexp
	| In of var * var * procexp
	| Out of var * valexp * procexp;;

