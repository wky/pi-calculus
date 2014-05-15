(* simpletype.mli *)

open Syntax

type mult = (* Multiplicity, described in Chapter 5 Linear Type*)
	  Unused
	| Once
	| Many

type imult = mult (* Input multiplicity*)
type omult = mult (* Output multiplicity *)

type stype = (* Simple type *)
	  TVar of int (* type variable *)
	| UnitT
	| BoolT
	| IntT
	| ChanT of stype
	(* | ChanT of stype * imult * omult (* channel type *) *)
	| PairT of stype * stype
	| FuncT of stype * stype (* function type: from -> to *)

type tenv = (var * stype) list
type constraints = (stype * stype) list
exception Unification of string

(* val new_tvar: unit -> stype
val type_inf: 'a procexp -> tenv -> stype procexp * constraints *)

val elaborate: unit procexp -> stype procexp * tenv
val string_of_type: stype -> string
