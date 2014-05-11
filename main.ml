(* main.ml *)
open Batteries

let main () = 
	let _ = print_string "Pi-Calculus Type Checker\n" in
	let filename = 
		try Sys.argv.(1)
		with Invalid_argument _ -> (
			print_string "Usage:\n  checker <file>\n"; 
			exit(-1)) in
	let in_strm = 
		try open_in filename
		with Sys_error _ -> (
			print_string ("Cannot open file: "^filename^"\n"); 
			exit(-1)) in
	let lex_buf = Lexing.from_channel in_strm in
	let proc =
		try Parser.main Lexer.token lex_buf
		with 
		  Failure _ -> exit(-1)
		| Parsing.Parse_error -> exit(-1)
	in print_string ((dump proc)^"\n");;

if !Sys.interactive then () else main();;