(* complextype.ml *)

open Syntax

type vint = Value of int | Variable of int | Expr of vint * vint | Infinity

type usage = 
	  NoUse
	| ParUse of usage * usage (* P|Q *)
	| InUse of vint * vint * usage
	| OutUse of vint * vint * usage
	| OrUse of usage * usage (* if cond then Q else Q *)
	| RepUse of usage (* not implemeted *)
	| UpUse of vint * usage

type diffcons = (vint * vint * int) list (* difference constraints, x1 - x2 <= n *)

exception NotImplemented
exception UnknownType

let string_of_vint v =
	let i2s = string_of_int in
	let rec v2s v = match v with
		  Value(i) -> i2s i
		| Variable(x) -> "Var"^(i2s x)
		| Expr(x, y) -> (v2s x)^"+"^(v2s y)
		| Infinity -> "inf"
	in v2s v

let string_of_usage u = 
	let v2s = string_of_vint in
	let rec u2s u = match u with
	  NoUse -> "NoUse"
	| ParUse(u1, u2) -> (u2s u1)^"|"^(u2s u2)
	| InUse(ob, cap, u') -> "I("^(v2s ob)^","^(v2s cap)^")."^(u2s u')
	| OutUse(ob, cap, u') -> "O("^(v2s ob)^","^(v2s cap)^")."^(u2s u')
	| OrUse(u1, u2) -> (u2s u1)^"&"^(u2s u2)
	| RepUse(u') -> "*"^(u2s u')
	| UpUse(v, u') -> "↑("^(v2s v)^","^(u2s u')^")"
in u2s u

let curr_vint = ref 1
let new_vint () = let v = !curr_vint in (curr_vint := v + 1; Variable(v))

let get_usage x env = try List.assoc x env with Not_found -> NoUse

let merge_env x u ncap env = 
	let not_x (x', _) = (x!=x') in
	let env' = List.filter not_x env in
	let uplevel = Expr(ncap, Value(1)) in
	let up (x', u') = (x', UpUse(uplevel, u')) in
	 (x, u)::(List.map up env')

let merge_env_aux merge_usage env1 env2 = 
	let merge_with_env2 (x, u) =
		try let u' = List.assoc x env2 in (x, merge_usage u u')
		with Not_found -> (x, u)
	and not_in_env1 (x, _) = not (List.exists (fun (x', _) -> x = x') env1) in
	let env1' = List.map merge_with_env2 env1 in
	let env2' = List.filter not_in_env1 env2 in
		env1' @ env2'

let merge_env_par env1 env2 = 
	let create_par u1 u2 = ParUse(u1, u2)
	in merge_env_aux create_par env1 env2
let merge_env_and env1 env2 = 
	let sub_usage u1 u2 =  OrUse(u1, u2)
	in merge_env_aux sub_usage env1 env2

(* stype procexp -> (var * usage) list * usage list *)
let rec type_check p = match p with
	  Zero -> ([], [])
	| Rep(p') -> 
		let (env, rel') = type_check p' in
		let rep_usage (x, u) = (x, RepUse(u)) in
		let env' = List.map rep_usage env in
			(env', rel')
	| Par(p1, p2) -> 
		let (env1, rel1) = type_check p1 in
		let (env2, rel2) = type_check p2 in
		let env' = merge_env_par env1 env2 in
			(env', rel1 @ rel2)
	| Nu(x, _, p', _) ->
		let (env, rel) = type_check p' in
		let u = get_usage x env in
		let not_x (x', _) = not (x=x') in
			(List.filter not_x env, u::rel)
	| If(_, p1, p2, _) ->
		let (env1, rel1) = type_check p1 in
		let (env2, rel2) = type_check p2 in
		let env' = merge_env_and env1 env2 in
			(env', rel1 @ rel2)
	| In(ch, x, _, p', _) ->
		let (env, rel) = type_check p' in
		let u = get_usage ch env in
		let ncap = new_vint() in
		let u' = InUse(Value(0), ncap, u) in
		let not_x (x', _) = (x'!=x) in
		let env' = List.filter not_x env in
		let env'' = merge_env ch u' ncap env' in
			(env'', rel)
	| Out(ch, _, p', _) ->
		let (env, rel) = type_check p' in
		let u = get_usage ch env in
		let ncap = new_vint() in
		let u' = OutUse(Value(0), ncap, u) in
		let env = merge_env ch u' ncap env in
		 (env, rel)

type restricted_usage =
	  RNoUse
	| RInUse of vint * vint * restricted_usage
	| ROutUse of vint * vint * restricted_usage
	| RParUse of restricted_usage list

let max_vint v1 v2 = match (v1, v2) with
	  (Value(0), _) -> v2
	| (_, Value(0)) -> v1
	| (_, _) -> failwith "wtf"

let rec normalize_usage u = match u with
	  NoUse -> RNoUse
	| ParUse(u1, u2) -> (
		let u1' = normalize_usage u1 in
		let u2' = normalize_usage u2 in
		match (u1', u2') with 
			  (RParUse(ul1), RParUse(ul2)) -> RParUse(ul1 @ ul2)
			| (RParUse(ul), _) -> RParUse(u2' :: ul)
			| (_, RParUse(ul)) -> RParUse(u1' :: ul)
			| (_, _) -> RParUse([u1';u2']) )
	| InUse(ob, cap, u') -> RInUse(ob, cap, normalize_usage u')
	| OutUse(ob, cap, u') -> ROutUse(ob, cap, normalize_usage u')
	| UpUse(v, u') -> (
		let up_usage u = (match u with
			  RInUse(ob, cap, u') -> RInUse(max_vint ob v, cap, u')
			| ROutUse(ob, cap, u') -> ROutUse(max_vint ob v, cap, u')
			| RNoUse -> u
			| _ -> failwith "wtf" )in
		let u'' = normalize_usage u' in (match u'' with 
			  RParUse(ul) -> RParUse(List.map up_usage ul)
			| _ -> up_usage u''))
	| _ -> raise NotImplemented

exception Impossible

type constraints = LT of vint * vint | MinLT of (vint list) * (vint list)

let partition_io ul = 
	let rec part il ol ul = match ul with 
		  (u::ul') -> (match u with RNoUse -> part il ol ul'
		  	| RInUse(_,_,_) -> part (u::il) ol ul'
			| ROutUse(_,_,_) -> part il (u::ol) ul'
			| RParUse(_) -> failwith "wtf")
		| _ -> (il, ol) in
	part [] [] ul

let extract_ob_cap ul = 
	let rec extract obl capl ul = match ul with
		  (u::ul') -> (match u with 
		  	  RInUse(ob, cap, _) -> extract (ob::obl) (cap::capl) ul'
		  	| ROutUse(ob, cap, _) -> extract (ob::obl) (cap::capl) ul'
			| _ -> failwith "wtf")
		| _ -> (obl, capl) in
	extract [] [] ul

let rec produce_constraints u = match u with
	  RNoUse -> []
	| RParUse(ul) ->
		let (il, ol) = partition_io ul in
		let (iob, icap) = extract_ob_cap il in
		let (oob, ocap) = extract_ob_cap ol in
		let c1 = [MinLT(iob, ocap);MinLT(oob, icap)] in
		if List.length il != 1 then raise NotImplemented else
		(* let (u1, u2) = (List.hd il, List.hd ol) in *)
		(* we should reduce u1 u2 further, but ... *)
			c1
	| RInUse(_, cap, u') -> LT(Infinity, cap) :: (produce_constraints u')
	| ROutUse(_, cap, u') -> LT(Infinity, cap) :: (produce_constraints u')


let elaborate p = 
	let (env, rel) = type_check p in
	let rel' = List.map normalize_usage rel in 
	let join c u = c @ (produce_constraints u) in
	let cl = List.fold_left join [] rel' in
		(env, cl)

let string_of_constraint c = 
	let v2s = string_of_vint in match c with 
		  LT(v1, v2) -> (v2s v1)^" <= "^(v2s v2)
		| MinLT(vl1, vl2) -> 
			let vl2s vl = 
				let sl = List.map v2s vl in
				let join s1 s2 = s1 ^ ", " ^ s2 in
					List.fold_left join (List.hd sl) (List.tl sl)
			in "Min("^(vl2s vl1)^") <= Min("^(vl2s vl2)^")" 

















