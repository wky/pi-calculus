%{
open Syntax
%}

%token <string> IDENT
%token <int> INT
%token <bool> BOOL

%token <Syntax.pos> INPUT
%token <Syntax.pos> OUTPUT
%token PAR
%token REP
%token <Syntax.pos> NU
%token IN
%token PERIOD
%token ZERO

%token <Syntax.pos> IF
%token THEN
%token ELSE
%token EQ
%token GT

%token LPAREN
%token RPAREN
%token COMMA

%token PLUS
%token MINUS

%token NIL
%token EOF
/*
%token <string> PRIM
*/

%nonassoc IN
%left PAR
%nonassoc ELSE
%nonassoc THEN
%right REP
%nonassoc PERIOD
%left COMMA
%left EQ GT
%left PLUS MINUS

%start main
%type <unit Syntax.procexp> main /*no type attatched yet*/
%type <unit Syntax.valexp> value
%type <string> pattern
%%

main:
  proc EOF { $1 }


value:
  IDENT { Var($1) }
| LPAREN RPAREN { Unit }
| BOOL { Bool($1) }
| INT { Int($1) }
/*| NIL { Prim("nil", Unit) }*/
| value PLUS value { Op("+", Pair($1, $3), ()) }
| value MINUS value { Op("-", Pair($1, $3), ()) }
| value EQ value { Op("==", Pair($1, $3), ()) }
| value GT value { Op(">", Pair($1, $3), ()) }
| LPAREN value RPAREN { $2 }
;

proc: ZERO { Zero }
| IDENT OUTPUT value PERIOD proc { Out($1, $3, $5, $2) }
| IDENT INPUT pattern PERIOD proc { In($1, $3, (), $5, $2) }
| proc PAR proc { Par($1, $3) }
| REP proc { Rep($2) }
| LPAREN proc RPAREN { $2 }
| NU IDENT IN proc { Nu($2, (), $4, $1) }
| IF value THEN proc ELSE proc { If($2, $4, $6, $1) }
;

pattern:
  IDENT { $1 }
;