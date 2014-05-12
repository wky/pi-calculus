(* lexer.mll *)
{
open Parser
open Lexing
let sym_table = [("true", Parser.BOOL(true)); ("false", Parser.BOOL(false))];;

let next_line lexbuf = 
	let pos = lexbuf.lex_curr_p in 
	lexbuf.lex_curr_p <- {
		pos with pos_bol = lexbuf.lex_curr_pos;
		pos_lnum = pos.pos_lnum + 1
	}

let lnum lexbuf = lexbuf.lex_curr_p.pos_lnum
let cnum lexbuf = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol
}

let space = [' ' '\t' '\r']
let newline = '\n'
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']


rule token = parse
| space+ { token lexbuf }
| newline { next_line lexbuf; token lexbuf }
| "/*" { comment lexbuf; token lexbuf }

| "?" { INPUT(lnum lexbuf, cnum lexbuf) }
| "!" { OUTPUT(lnum lexbuf, cnum lexbuf) } 
| "|" { PAR } 
| "*" { REP }
| "new" { NU(lnum lexbuf, cnum lexbuf) }
| "in" { IN }
| "." { PERIOD }
| "O" { ZERO }

| "if" { IF(lnum lexbuf, cnum lexbuf) }
| "then" { THEN }
| "else" { ELSE }
| "==" { EQ }
| ">" { GT }

| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }

| "+" { PLUS }
| "-" { MINUS }

(*
| "nil" { NIL }
| "isnil" { OP("isnil")}
| "cons" { OP("cons") }
| "hd" { OP("hd") }
| "tl" { OP("tl")}
*)

| digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
| lower (lower|upper|digit|'_')* { let s = Lexing.lexeme lexbuf in try List.assoc s sym_table with Not_found -> IDENT(s) }
| eof { EOF }

| _ { failwith ("unknown token: "^(Lexing.lexeme lexbuf)) }

and comment = parse

| "*/" { () }
| "/*" { comment lexbuf; comment lexbuf; }
| eof { failwith "unterminated comment" }
| _ { comment lexbuf }



