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
%token SEMICOLON
%token DOT
%token COMMA
%token CARET
%token ASSIGN
%token ASTERISK
%token MINUS

(* Parentheses *)

%token LPAREN RPAREN
%token LBLOCK RBLOCK
%token LBRACK RBRACK

(* Keywords *)

%token IF ELSE

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
  | ELSE IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK
    { Else (`Cond (If (e, b))) }
  | ELSE LBLOCK; b = statement* RBLOCK { Else (`NoCond b) }
  | t=IDENT ASTERISK? s=IDENT SEMICOLON { NewVar (t, s, Atom Null) }
  | t=IDENT ASTERISK? s=IDENT ASSIGN e = expr SEMICOLON { NewVar (t, s, e) }
  | s=IDENT ASSIGN e = expr SEMICOLON { Mutate (s, e) }
  | SELF DOT s=IDENT ASSIGN e = expr SEMICOLON { Mutate (s, e) }
  | s=COMMENT { Comment s }
  | e = expr SEMICOLON { Exec e }

expr:
  | LPAREN; e = expr RPAREN { Expr e }
  | LBRACK; e = expr s=IDENT RBRACK { Message (e, s, []) }
  | LBRACK; e = expr; l = list(s=IDENT COLON; e = expr { (s, e) }) RBRACK
    { make_message e l }
  | CARET; t = ioption(LPAREN s=IDENT ASTERISK? RPAREN { s })
    LBLOCK; b = statement* RBLOCK { Block (t, [], b) }
  | CARET; t = ioption(LPAREN s=IDENT ASTERISK? RPAREN { s })
    LPAREN l = separated_list(COMMA, t=IDENT ASTERISK* s=IDENT { (t, s) }) RPAREN
    LBLOCK; b = statement* RBLOCK { Block (t, l, b) }
  | e1 = expr; op = binop; e2 = expr { Binary (op, e1, e2) }
  | NOT; e = expr { Unary (Not, e) }
  | a = atom { Atom a }

%inline binop:
  | EQU { Equal }
  | NEQ { NotEqual }
  | LES { Less }
  | GRT { Greater }
  | LEQ { LessEqual }
  | GEQ { GreaterEqual }
  | AND { And }
  | OR { Or }

atom:
  | i=INT { Int i }
  | f=FLOAT { Float f }
  | s=IDENT { Var s }
  | SELF DOT s=IDENT { Prop s }
  | SELF { Self }
  | NULL { Null }
  | NIL { Nil }
