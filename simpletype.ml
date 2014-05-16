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

let new_tvar () = let x = !curr_tvar in (curr_tvar := x + 1; TVar(x))

(* ('a procexp -> tenv) -> stype procexp * constraints *)
let type_constraints p env =
	let rec tinfp p env = match p with
		  Zero -> (Zero, [])
		| Rep(p1) ->
			let (p1', c1) = tinfp p1 env in
				(Rep(p1'), c1)
		| Par(p1, p2) ->
			let (p1', c1) = tinfp p1 env in
			let (p2', c2) = tinfp p2 env in
				(Par(p1', p2'), c1 @ c2)
		| Nu(x, _, p1, pos) ->
			let (tx, env1) = extend_tenv x env in
			let (p1', c1) = tinfp p1 env1 in
			let c = (tx, ChanT(new_tvar()))::c1 in
				(Nu(x, tx, p1', pos), c)
		| If(cond, p1, p2, pos) ->
			let (p1', c1) = tinfp p1 env in
			let (p2', c2) = tinfp p2 env in
			let (cond', ty, c3) = tinfv cond env in
			let c = (ty, BoolT) ::  (c1  @ c2 @ c3) in
				(If(cond', p1', p2', pos), c)
		| In(ch, x, _, p1, pos) ->
			let (tx, env1) = extend_tenv x env in
			let (p1', c1) = tinfp p1 env1 in
			let tch = List.assoc ch env in
			let c = (tch, ChanT(tx))::c1 in
				(In(ch, x, tx, p1', pos), c)
		| Out(ch, v1, p1, pos) ->
			(* let _ = print_endline ("out:" ^ ch ^ "@"^(string_of_int (fst pos))^","^(string_of_int (snd pos)))in *)
			let (v1', t1, c1) = tinfv v1 env in
			let (p1', c2) = tinfp p1 env in
			let tch = List.assoc ch env in
			let c = (tch, ChanT(t1))::(c1 @ c2) in
				(Out(ch, v1', p1', pos), c)
	and tinfv v env = match v with
		  Unit -> (Unit, UnitT, [])
		| Var(x) -> (Var(x), List.assoc x env, [])
		| Bool(b) -> (Bool(b), BoolT, [])
		| Int(i) -> (Int(i), IntT, [])
		| Op(op, _, pos) ->
			let ty = lookup_op op in
				(Op(op, ty, pos), ty, [])
		| App(func, arg) ->
			let (func', t1, c1) = tinfv func env in
			let (arg', t2, c2) = tinfv arg env in
			let t3 = new_tvar() in
			let c = (t1, FuncT(t2, t3))::(c1 @ c2) in
				(App(func', arg'), t3, c)
		| Pair(v1, v2) ->
			let (v1', t1, c1) = tinfv v1 env in
			let (v2', t2, c2) = tinfv v2 env in
				(Pair(v1', v2'), PairT(t1, t2), c1 @ c2)
	and extend_tenv x env = let tx = new_tvar() in (tx, (x, tx)::env)
	and lookup_op op = let globals =
		[("+", FuncT(PairT(IntT, IntT), IntT));
		 ("-", FuncT(PairT(IntT, IntT), IntT));
		 ("==", FuncT(PairT(IntT, IntT), BoolT));
		 (">", FuncT(PairT(IntT, IntT), BoolT));]
	in List.assoc op globals
in tinfp p env

let type_subst s t =
	let rec tsubst s t = match s with [] -> t | (t1, t2)::s1 ->
		match t with
		  TVar x -> tsubst s1 (apply1 t1 t2 x)
		| ChanT t' -> ChanT (tsubst s t')
		| PairT(t1, t2) -> PairT(tsubst s t1, tsubst s t2)
		| FuncT(t1, t2) -> FuncT(tsubst s t1, tsubst s t2)
		| _ -> t
	and apply1 t1 t2 x = if t1=x then t2 else TVar x
in tsubst s t


exception Unification of string
(* constraints -> substutions *)
let unification c =
	let rec unify c s = match c with [] -> s | (t1, t2) :: c1 ->
		if t1 = t2 then unify c1 s else match (t1, t2) with
		  (TVar x, _) -> if occurs x t2 then raise (Unification "recursive type?") else
			let s1 = [(x, t2)] in
			let trans = fun (t1, t2) -> (type_subst s1 t1, type_subst s1 t2) in
			let c1' = List.map trans c1 in
				unify c1' (s1 @ s)
		| (_, TVar _) -> unify ((t2, t1)::c1) s (* change order *)
		| (ChanT t1', ChanT t2') -> unify ((t1', t2')::c1) s
		| (PairT(t11, t12), PairT(t21, t22)) -> unify ((t11, t21)::(t12, t22)::c1) s
		| (FuncT(t11, t12), FuncT(t21, t22)) -> unify ((t11, t21)::(t12, t22)::c1) s
		| _ -> raise (Unification "unknown constraints")
	and occurs x t = match t with
		  TVar y -> x = y
		| ChanT t' -> occurs x t'
		| PairT(t1, t2) -> (occurs x t1) || (occurs x t2)
		| FuncT(t1, t2) -> (occurs x t1) || (occurs x t2)
		| _ -> false
	in List.rev(unify c [])

(* unit procexp -> stype procexp * tenv *)
let elaborate p =
	let fv_env = List.map (fun fv -> (fv, new_tvar())) (freevars p) in
	let (p', c) = type_constraints p fv_env in
	let s = unification c in
	let fv_env' = List.map (fun (fv, t) -> (fv, type_subst s t)) fv_env in
	let p'' = map_type (type_subst s) p' in
		(p'', fv_env')

let string_of_type t =
	let i2s = string_of_int in
	let rec t2s t = match t with
		  TVar(x) -> "TVar"^(i2s x)
		| UnitT -> "Unit"
		| BoolT -> "Bool"
		| IntT -> "IntT"
		| ChanT t1 -> "Chan("^(t2s t1)^")"
		| PairT(t1, t2) -> (t2s t1)^"*"^(t2s t2)
		| FuncT(t1, t2) -> (t2s t1)^"->"^(t2s t2)
in t2s t
