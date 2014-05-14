(* syntax.ml *)
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

module Varset = Set.Make(struct type t = string;; let compare = compare end)

let alphaconv_proc p = 
	let rec aconv p s = match p with
	  Zero -> Zero
	| Rep(p1) -> Rep(aconv p1 s)
	| Par(p1, p2) -> Par(aconv p1 s, aconv p2 s)
	| Nu(x, ty, p1, pos) -> 
		let x' = new_var x pos in
		let s' = make_subst s x x' in
			Nu(x', ty, aconv p1 s', pos)
	| If(cond, p1, p2, pos) ->
		let cond' = subst_value s cond in
			If(cond', aconv p1 s, aconv p2 s, pos)
	| In(ch, x, ty, p1, pos) ->
		let x' = new_var x pos in
		let s' = make_subst s x x' in
			In(lookup_subst s ch, x', ty, aconv p1 s', pos)
	| Out(ch, v1, p1, pos) ->
		Out(lookup_subst s ch, subst_value s v1, aconv p1 s, pos)
	and new_var x pos = x ^ "/" ^ (string_of_int (fst pos)) ^ "-" ^ (string_of_int (snd pos))
	and make_subst s x x' = (x, x') :: s
	and lookup_subst s x = try List.assoc x s with Not_found -> x
	and subst_value s v = match v with
	  Var(x) -> Var(lookup_subst s x)
	| App(v1, v2) -> App(subst_value s v1, subst_value s v2)
	| Pair(v1, v2) -> Pair(subst_value s v1, subst_value s v2)
	| _ -> v
in aconv p []


let fvlist_proc p =

	let rec fv_proc p = match p with
	  Zero -> Varset.empty
	| Rep(p1) -> fv_proc p1
	| Par(p1, p2) -> Varset.union (fv_proc p1) (fv_proc p2)
	| Nu(x, _, p1, _) -> Varset.remove x (fv_proc p1)
	| If(cond, p1, p2, _) -> Varset.union (Varset.union (fv_proc p1) (fv_proc p2)) (fv_value cond)
	| In(ch, x, _, p1, _) -> Varset.add ch (Varset.remove x (fv_proc p1))
	| Out(ch, v1, p1, _) -> Varset.add ch (Varset.union (fv_value v1) (fv_proc p1))

	and fv_value v = match v with
	  Var(x) -> Varset.singleton x
	| App(v1, v2) -> Varset.union (fv_value v1) (fv_value v2)
	| Pair(v1, v2) -> Varset.union (fv_value v1) (fv_value v2)
	| _ -> Varset.empty

in Varset.elements (fv_proc p)