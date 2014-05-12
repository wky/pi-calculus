(* syntax.ml *)
type var = string
type pos = int * int
type op = string

type 'ty valexp = Unit
	| Var of var
	| Bool of bool
	| Int of int
	| Op of op * 'ty valexp * 'ty
	| Pair of 'ty valexp * 'ty valexp;;

type 'ty procexp = Zero
	| Par of 'ty procexp * 'ty procexp
	| Rep of 'ty procexp
	| Nu of var * 'ty * 'ty procexp * pos
	| If of 'ty valexp * 'ty procexp * 'ty procexp * pos
	| In of var * var * 'ty * 'ty procexp * pos
	| Out of var * 'ty valexp * 'ty procexp * pos;;

