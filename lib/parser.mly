%{
    open Ast
%}

(* Atoms *)

%token <int> INT
%token <float> FLOAT
%token NULL
%token SELF NIL
%token <string> COMMENT

%token COLON
%token ASTERISK
%token MINUS
%token ASSIGN
%token SEMICOLON
%token DOT

(* Parentheses *)

%token LPAREN RPAREN
%token LBLOCK RBLOCK
%token LBRACK RBRACK

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
  | t=IDENT ASTERISK? s=IDENT SEMICOLON { NewVar (t, s, Atom Null) }
  | t=IDENT ASTERISK? s=IDENT ASSIGN e = expr SEMICOLON { NewVar (t, s, e) }
  | s=IDENT ASSIGN e = expr SEMICOLON { Mutate (s, e) }
  | SELF DOT s=IDENT ASSIGN e = expr SEMICOLON { Mutate (s, e) }
  | s=COMMENT { Comment s }
  | e = expr SEMICOLON { Expr e }

expr:
  | LPAREN; e = expr RPAREN { Expr e }
  | LBRACK; e = expr s=IDENT RBRACK { Message (e, s, []) }
  | LBRACK; e = expr; l = list(s=IDENT COLON; e=expr { (s, e) }) RBRACK
    { Message (e, List.hd l |> fst, l) }
  | e1 = expr EQU; e2 = expr { Binary (Equal, e1, e2) }
  | e1 = expr NEQ; e2 = expr { Binary (NotEqual, e1, e2) }
  | e1 = expr LES; e2 = expr { Binary (Less, e1, e2) }
  | e1 = expr GRT; e2 = expr { Binary (Greater, e1, e2) }
  | e1 = expr LEQ; e2 = expr { Binary (LessEqual, e1, e2) }
  | e1 = expr GEQ; e2 = expr { Binary (GreaterEqual, e1, e2) }
  | e1 = expr AND; e2 = expr { Binary (And, e1, e2) }
  | e1 = expr OR; e2 = expr { Binary (Or, e1, e2) }
  | NOT; e = expr { Unary (Not, e) }
  | a = atom { Atom a }

atom:
  | i=INT { Int i }
  | f=FLOAT { Float f }
  | s=IDENT { Var s }
  | SELF DOT s=IDENT { Prop s }
  | SELF { Self }
  | NULL { Null }
  | NIL { Nil }
