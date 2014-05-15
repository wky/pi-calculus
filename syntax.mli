(* syntax.mli *)
type var = string (* Variable, represented by it's identifier *)
type pos = int * int (* Position in source code: line * column *)
type op = string (* Operator,  represented by it's identifier *)

type 'ty valexp = 	(* Expression of Value annotated with type 'ty *)
	  Unit 			(* Unit value: "()" *)
	| Var of var 	(* Variable *)
	| Bool of bool 	(* Boolean value *)
	| Int of int 	(* Integer *)
	| Op of op * 'ty * pos (* Operator, of type 'ty, at pos *)
	| App of 'ty valexp * 'ty valexp (* Application: function * argument *)
	| Pair of 'ty valexp * 'ty valexp (* Pair: fst * snd *)

type 'ty procexp = (* Expression of Processes annotated with type 'ty *)
	  Zero (* Zero process *)
	| Rep of 'ty procexp (* Infinite Repetition of a Process *)
	| Par of 'ty procexp * 'ty procexp (* Parallel of two processes *)
	| Nu of var * 'ty * 'ty procexp * pos (* New Channel and type of the channel *)
	| If of 'ty valexp * 'ty procexp * 'ty procexp * pos (* If-Then-Else *)
	| In of var * var * 'ty * 'ty procexp * pos 	(* Input from channel, bind value to variable with type *)
	| Out of var * 'ty valexp * 'ty procexp * pos 	(* Output to channel, with value *)

val alphaconversion: 'a procexp -> 'a procexp
val freevars: 'a procexp -> var list
val map_type: ('a -> 'a) -> 'a procexp -> 'a procexp
val string_of_proc: ('a -> string) -> 'a procexp -> string
