(* main.ml *)
open Syntax;;
open Lexing;;
let rec value_to_str value :string = match value with
  Unit -> "Unit"
| Var(v) -> v
| Bool(b) -> string_of_bool b
| Int(i) -> string_of_int i
| Op(op, v) -> op ^ (value_to_str v)
| Pair(v1, v2) -> "("^(value_to_str v1)^", "^(value_to_str v2)^")"

let rec proc_to_str proc :string = match proc with 
  Zero -> "Zero"
| Par(p1, p2) -> "Parallel("^(proc_to_str p1)^", "^(proc_to_str p2)^")"
| Rep(p) -> "Repeat("^(proc_to_str p)^")"
| Nu(var, p) -> "New("^var^", "^(proc_to_str p)^")"
| If(cond, p1, p2) -> "If ["^(value_to_str cond)^" Then "^(proc_to_str p1)^" Else "^(proc_to_str p2)
| In(chan, var, p) -> "In "^chan^"("^var^")."^(proc_to_str p)
| Out(chan, value, p) -> "Out "^chan^"("^(value_to_str value)^")."^(proc_to_str p)

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
		| Parsing.Parse_error -> ( show_syntax_error lex_buf; exit(-1))
	in print_string ((proc_to_str proc)^"\n");;

if !Sys.interactive then () else main();;