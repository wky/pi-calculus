(* simpletype.ml *)

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

let curr_tvar = ref 1

let new_tvar () = 
	let x = !curr_tvar in (curr_tvar := x + 1; TVar(x)) 

let extend_tenv x env = 
	let tx = new_tvar() in (tx, (x, tx)::env)

let lookup_tenv x env = List.assoc x env

let globals = [("+", FuncT(PairT(IntT, IntT), IntT));
			   ("-", FuncT(PairT(IntT, IntT), IntT));
			   ("==", FuncT(PairT(IntT, IntT), BoolT));
			   (">", FuncT(PairT(IntT, IntT), BoolT));]

let lookup_op op = List.assoc op globals

let merge_constraints clist = List.fold_left (@) [] clist

(* ('a valexp -> tenv) -> stype valexp * stype * constraints *)
let rec type_inf_val v env = match v with
	  Unit -> (Unit, UnitT, [])
	| Var(x) -> (Var(x), lookup_tenv x env, [])
	| Bool(b) -> (Bool(b), BoolT, [])
	| Int(i) -> (Int(i), IntT, [])
	| Op(op, _, pos) ->
		let ty = lookup_op op in
			(Op(op, ty, pos), ty, [])
	| App(func, arg) ->
		let (func', t1, c1) = type_inf_val func env in
		let (arg', t2, c2) = type_inf_val arg env in
		let t3 = new_tvar() in
		let c = (t1, FuncT(t2, t3))::(c1 @ c2) in
			(App(func', arg'), t3, c)
	| Pair(v1, v2) -> 
		let (v1', t1, c1) = type_inf_val v1 env in
		let (v2', t2, c2) = type_inf_val v2 env in
			(Pair(v1', v2'), PairT(t1, t2), c1 @ c2)

(* ('a procexp -> tenv) -> stype procexp * constraints *)
let rec type_inf p env = match p with
	  Zero -> (Zero, [])
	| Rep(p1) -> 
		let (p1', c1) = type_inf p1 env in 
			(Rep(p1'), c1)
	| Par(p1, p2) -> 
		let (p1', c1) = type_inf p1 env in
		let (p2', c2) = type_inf p2 env in
			(Par(p1', p2'), c1 @ c2)
	| Nu(x, _, p1, pos) ->
		let (tx, env1) = extend_tenv x env in
		let (p1', c1) = type_inf p1 env1 in
		let c = (tx, ChanT(new_tvar()))::c1 in
			(Nu(x, tx, p1', pos), c)
	| If(cond, p1, p2, pos) ->
		let (p1', c1) = type_inf p1 env in
		let (p2', c2) = type_inf p2 env in
		let (cond', ty, c3) = type_inf_val cond env in
		let c = (ty, BoolT) :: merge_constraints [c1;c2;c3] in
			(If(cond', p1', p2', pos), c)
	| In(ch, x, _, p1, pos) ->
		let (tx, env1) = extend_tenv x env in
		let (p1', c1) = type_inf p1 env1 in
		let tch = lookup_tenv ch env in
		let c = (tch, ChanT(tx))::c1 in
			(In(ch, x, tx, p1', pos), c)
	| Out(ch, v1, p1, pos) ->
		let (v1', t1, c1) = type_inf_val v1 env in
		let (p1', c2) = type_inf p1 env in
		let tch = lookup_tenv ch env in
		let c = (tch, ChanT(t1)) :: (c1 @ c2) in
			(Out(ch, v1', p1', pos), c)


(* unit procexp -> stype procexp * tenv *)
let elaborate p = ()