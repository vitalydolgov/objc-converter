%{
    open Ast
%}

(* Atoms *)

%token <int> INT
%token <float> FLOAT

%token COLON
%token ASTERISK
%token MINUS

(* Parentheses *)

%token LPAREN RPAREN
%token LBLOCK RBLOCK

(* Keywords *)

%token IF

(* Operators *)

%token AND OR

%token NOT
%token EQU NEQ LEQ GEQ
%token LES GRT

(* Other *)

%token <string> IDENT
%token EOF

%left OR
%left AND
%left EQU NEQ LEQ GEQ LES GRT
%left NOT

(* Types *)

%type <Ast.program> program

%start program

%%

program:
  | declar* EOF { Program $1 }
;

declar:
  | m = method_comp* LBLOCK; b = statement* RBLOCK
    { make_method m b }

method_comp:
  | MINUS LPAREN s=IDENT ASTERISK? RPAREN { Return_type s }
  | LPAREN s=IDENT ASTERISK? RPAREN { Type s }
  | s=IDENT COLON { Label s }
  | s=IDENT { Identifier s }

statement:
  | IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK { If (e, b) }
  | INT { Dummy }

expr:
  | LPAREN; e = expr RPAREN { Expr e }
  | NOT; e = expr { Unary(Not, e) }
  | e1 = expr EQU; e2 = expr { Binary (Equal, e1, e2) }
  | e1 = expr NEQ; e2 = expr { Binary (NotEqual, e1, e2) }
  | e1 = expr LES; e2 = expr { Binary (Less, e1, e2) }
  | e1 = expr GRT; e2 = expr { Binary (Greater, e1, e2) }
  | e1 = expr LEQ; e2 = expr { Binary (LessEqual, e1, e2) }
  | e1 = expr GEQ; e2 = expr { Binary (GreaterEqual, e1, e2) }
  | e1 = expr AND; e2 = expr { Binary (And, e1, e2) }
  | e1 = expr OR; e2 = expr { Binary (Or, e1, e2) }
  | a = atom { Atom a }

atom:
  | i=INT { Int i }
  | s=IDENT { Var s }
