(* syntax.ml *)
type var = string
type pos = int * int
type op = string

type valexp = Unit
	| Var of var
	| Bool of bool
	| Int of int
	| Op of op * valexp
	| Pair of valexp * valexp;;

type procexp = Zero
	| Par of procexp * procexp
	| Rep of procexp
	| Nu of var * procexp * pos
	| If of valexp * procexp * procexp * pos
	| In of var * var * procexp * pos
	| Out of var * valexp * procexp * pos;;

