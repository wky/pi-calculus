%{
open Syntax
%}

%token <string> IDENT
%token <int> INT
%token <bool> BOOL

%token INPUT
%token OUTPUT
%token PAR
%token REP
%token NU
%token IN
%token PERIOD
%token ZERO

%token IF
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
%type <Syntax.procexp> main
%type <Syntax.valexp> val

%%

main:
  proc EOF { $1 }
;

proc: ZERO { Zero }
| IDENT OUTPUT val PERIOD proc { Out(%1, %3, %5) }
| IDENT INPUT IDENT PERIOD proc { In(%1, %3, %5) }
| proc PAR proc { Par(%1, %3) }
| REP proc { Rep(%2) }
| LPAREN proc RPAREN { $2 }
| NU IDENT IN proc { Nu(%2, %4) }
| IF val THEN proc ELSE proc { If(%2, %4, %6) }
;

val:
  IDENT { Var(IDENT) }
| LPAREN RPAREN { Unit }
| BOOL { Bool(%1) }
| INT { Int(%1) }
| NIL { Prim("nil", Unit) }
| val PLUS val { Prim("+", Pair(%1, %3)) }
| val MINUS val { Prim("-", Pair(%1, %3)) }
| val EQ val { Prim("==", Pair(%1, %3)) }
| val GT val { Prim(">", Pair(%1, %3)) }
| LPAREN val RPAREN { %2 }
;