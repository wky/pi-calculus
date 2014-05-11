(* lexer.mll *)
{
open Parser
exception Eof
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']


rule token = parse
| space+ { token lexbuf }
| "/*" { comment lexbuf; token lexbuf }

| "?" { INPUT }
| "!" { OUTPUT } 
| "|" { PAR } 
| "*" { REP }
| "new" { NU }
| "in" { IN }
| "." { PERIOD }
| "O" { ZERO }

| "if" { IF }
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

| digit+ { INT() }
| lower (lower|upper|digit|'_')* { IDENT() }
| eof { EOF }

| _ { failwith "lex error" }

and comment = parse

| "*/" { () }
| "/*" { comment lexbuf; comment lexbuf; }
| eof { failwith "unterminated comment" }
| _ { comment lexbuf }



