%{
    open Ast
%}

(* Atoms *)

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token NULL
%token YES NO
%token SELF NIL
%token CLASS
%token <string> COMMENT
%token <string> IGNORE

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
%token RETURN

(* Operators *)

%token AND OR

%token NOT
%token EQU NEQ LEQ GEQ
%token LESS GREATER

(* Raw types *)

%token ID
%token VOID

(* Other *)

%token <string> IDENT
%token <string> TYPE_IDENT
%token <string> CONSTANT
%token <string> SELECTOR
%token EOF

%left OR
%left AND
%left EQU NEQ LEQ GEQ LESS GREATER
%right NOT
%left DOT

%nonassoc LBRACK

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
  | MINUS LPAREN t = typ RPAREN { Return_type t }
  | LPAREN; t = typ RPAREN { Param_type t }
  | s=IDENT COLON { Label s }
  | s=IDENT { Identifier s }

statement:
  | IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK { If (e, b) }
  | ELSE IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK
    { Else (`Cond (If (e, b))) }
  | ELSE LBLOCK; b = statement* RBLOCK { Else (`NoCond b) }
  | t = typ s=IDENT SEMICOLON { NewVar (t, s, Atom Null) }
  | t = typ s=IDENT ASSIGN e = expr SEMICOLON { NewVar (t, s, e) }
  | e1 = expr ASSIGN; e2 = expr SEMICOLON { Mutate (e1, e2) }
  | s=COMMENT { Comment s }
  | e = expr SEMICOLON { Exec e }
  | RETURN; e = expr? SEMICOLON { Return e }

typ:
  | ID { make_type "id" None }
  | VOID { make_type "void" None }
  | s=TYPE_IDENT LESS g=TYPE_IDENT GREATER ASTERISK { make_type s (Some g) }
  | s=TYPE_IDENT ASTERISK { make_type s None }

expr:
  | LPAREN; e = expr RPAREN { Expr e }
  | LBRACK; e = expr ID RBRACK | e = expr DOT ID { Property (e, "id") }
  | LBRACK; e = expr s=IDENT RBRACK { Message (e, s, []) }
  | LBRACK; e = expr; l = list(s=IDENT COLON; e = expr { (s, e) }) RBRACK
    { make_message e l }
  | CARET; i = ioption(t = typ { t }) LBLOCK; b = statement* RBLOCK
    { let t = match i with Some t -> t | None -> Void in
      Block (t, [], b) }
  | CARET; i = ioption(t = typ { t })
    LPAREN l = separated_list(COMMA, t = typ s=IDENT { (t, s) }) RPAREN
    LBLOCK; b = statement* RBLOCK
    { let t = match i with Some t -> t | None -> Void in
      Block (t, l, b) }
  | e1 = expr; op = binop; e2 = expr { Binary (op, e1, e2) }
  | NOT; e = expr { Unary (Not, e) }
  | i = endrule( e = expr; DOT s=IDENT { (e, s) }) { Property (fst i, snd i) }
  | e1 = expr LBRACK; e2 = expr RBRACK { Element(e1, e2) }
  | LPAREN; t = typ RPAREN s=IDENT { TypeCast(t, s) }
  | s=IDENT LPAREN; e = expr RPAREN { Func1(s, e) }
  | a = atom { Atom a }

%inline binop:
  | EQU { Equal }
  | NEQ { NotEqual }
  | LESS { Less }
  | GREATER { Greater }
  | LEQ { LessEqual }
  | GEQ { GreaterEqual }
  | AND { And }
  | OR { Or }

atom:
  | s=IGNORE { Ignore s }
  | i=INT { Int i }
  | f=FLOAT { Float f }
  | s=STRING { String s }
  | s=TYPE_IDENT DOT CLASS
  | s=TYPE_IDENT DOT SELF { TypeRef s }
  | s=CONSTANT { Constant s }
  | s=SELECTOR { Selector s }
  | s=IDENT { Var s }
  | SELF { Self }
  | NULL { Null }
  | NIL { Nil }
  | YES { Bool true }
  | NO { Bool false }
