(* main.ml *)
open Syntax;;
open Lexing;;
let rec string_of_val value :string = match value with
  Unit -> "Unit"
| Var(v) -> v
| Bool(b) -> string_of_bool b
| Int(i) -> string_of_int i
| Op(op, ty, pos) -> op
| App(v1, v2) -> (string_of_val v1)^(string_of_val v2)
| Pair(v1, v2) -> "("^(string_of_val v1)^", "^(string_of_val v2)^")"

let rec string_of_proc proc :string = match proc with 
  Zero -> "Zero"
| Par(p1, p2) -> "Parallel("^(string_of_proc p1)^", "^(string_of_proc p2)^")"
| Rep(p) -> "Repeat("^(string_of_proc p)^")"
| Nu(var, ty,p, pos) -> "New("^var^", "^(string_of_proc p)^")"
| If(cond, p1, p2, pos) -> "If ["^(string_of_val cond)^" Then "^(string_of_proc p1)^" Else "^(string_of_proc p2)
| In(chan, var, ty, p, pos) -> "In["^(string_of_int (fst pos))^","^(string_of_int (snd pos))^"]"^chan^"("^var^")."^(string_of_proc p)
| Out(chan, value, p, pos) -> "Out["^(string_of_int (fst pos))^","^(string_of_int (snd pos))^"]"^chan^"("^(string_of_val value)^")."^(string_of_proc p)

let show_syntax_error lex_buf = (
	print_endline ("syntax error at character "^(string_of_int lex_buf.Lexing.lex_curr_p.pos_cnum));
	print_endline ("re-run with environment variable OCAMLRUNPARAM='p' to reveal errors")
)

let show_lexing_error lex_buf failure = (
	print_endline ("lexing error at character "^(string_of_int lex_buf.Lexing.lex_curr_p.pos_cnum));
	print_endline failure
)

let main () = 
	let _ = print_string "Pi-Calculus Type Checker\n" in
	let filename = 
		try Sys.argv.(1)
		with Invalid_argument _ -> (
			print_string "Usage:  checker <file>\n"; 
			exit(-1)) in
	let in_strm = 
		try open_in filename
		with Sys_error _ -> (
			print_string ("Cannot open file: "^filename^"\n"); 
			exit(-1)) in
	let lex_buf = Lexing.from_channel in_strm in
	let proc = try Parser.main Lexer.token lex_buf 
		with Failure f -> (show_lexing_error lex_buf f; exit(-1))
		| Parsing.Parse_error -> (show_syntax_error lex_buf; exit(-1)) in
	let (proc', _) = Simpletype.type_inf proc [] in
	let proc_str = string_of_proc (alphaconv_proc proc') in
	let fv_str = List.fold_left (^) "" (fvlist_proc proc) in (
		print_endline proc_str;
		print_endline fv_str);;

if !Sys.interactive then () else 
	try (
		Printexc.record_backtrace true;
		main() )
	with exn -> Printexc.print_backtrace stdout;;