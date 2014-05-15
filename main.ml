(* main.ml *)
open Syntax;;
open Lexing;;

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
	let p = try Parser.main Lexer.token lex_buf
		with Failure f -> (show_lexing_error lex_buf f; exit(-1))
		| Parsing.Parse_error -> (show_syntax_error lex_buf; exit(-1)) in
	let p' = alphaconversion p in
	let (p'', env) = Simpletype.elaborate p' in
	let t2s = Simpletype.string_of_type in
	let proc_str = string_of_proc t2s p'' in
	let print_fv = fun (fv, ty) -> print_endline (fv^":"^(t2s ty))
in (print_endline proc_str; List.iter print_fv env);;

if !Sys.interactive then () else 
	try (
		Printexc.record_backtrace true;
		main() )
	with _ -> Printexc.print_backtrace stdout;;
