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
%token <string> TYPEREF
%token <string * string> GENTYPE
%token <string> COMMENT
%token <string> IGNORE

%token COLON
%token SEMICOLON
%token DOT
%token COMMA
%token CARET
%token AT
%token ASTERISK
%token MINUS
%token PLUS

%token ASSIGN

(* Parentheses *)

%token LPAREN RPAREN
%token LBLOCK RBLOCK
%token LBRACK RBRACK

(* Keywords *)

%token IF ELSE
%token FOR IN
%token RETURN

(* Operators *)

%token AND OR

%token NOT
%token EQU NEQ LEQ GEQ
%token LESS GREATER

(* Raw types *)

%token ID

(* Other *)

%token <string> IDENT
%token <string> SELECTOR
%token EOF

%left ASSIGN
%left PLUS MINUS
%left OR
%right NOT
%left LESS GREATER
%left LEQ GEQ EQU NEQ
%left DOT
%left AND
%right LBRACK
%right RPAREN

%type <Ast.program> program
%type <Ast.statement> assign
%type <Ast.typ> reftype

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
  | LPAREN; t = typ RPAREN s = IDENT { Param (t, s) }
  | s=IDENT COLON { Label s }
  | s=IDENT { Identifier s }

statement:
  | IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK { If (e, b) }
  | IF LPAREN; e = expr RPAREN s = statement { If (e, [s]) }
  | ELSE IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK
    { Else (`Cond (If (e, b))) }
  | ELSE LBLOCK; b = statement* RBLOCK { Else (`NoCond b) }
  | t = typ s=IDENT SEMICOLON { NewVar (t, s, Atom Null) }
  | t = typ s=IDENT ASSIGN e = expr SEMICOLON { NewVar (t, s, e) }
  | s=COMMENT { Comment s }
  | e = expr SEMICOLON { Exec e }
  | RETURN; e = expr? SEMICOLON { Return e }
  | FOR LPAREN; t = typ x=IDENT IN; e = expr RPAREN LBLOCK b = statement* RBLOCK
    { ForEach (t, x, e, b) }
  | FOR LPAREN; s = assign SEMICOLON e1 = expr; SEMICOLON e2 = expr RPAREN LBLOCK b = statement* RBLOCK
    { For (s, e1, e2, b) }

assign:
  | t = typ s=IDENT ASSIGN e = expr { NewVar (t, s, e) }

typ:
  | t = reftype { t }
  | s=IDENT { make_type s }

reftype:
  | ID { make_type "id" }
  | s=IDENT ASTERISK { make_type s }
  | p=GENTYPE ASTERISK { make_generic_type (fst p) (snd p) }

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
  | e = expr PLUS PLUS { Mutate (e, Incr, Atom (Int 1)) }
  | e = expr MINUS MINUS { Mutate (e, Decr, Atom (Int 1)) }
  | e = expr; DOT s=IDENT { Property (e, s) }
  | e1 = expr LBRACK; e2 = expr RBRACK { Element(e1, e2) }
  | s=IDENT LPAREN; l = separated_list(COMMA, e = expr { e }) RPAREN { Func(s, l) }
  | AT LBRACK l = separated_list(COMMA, a = atom { a }) RBRACK { Array l }
  | a = atom { Atom a }
  | LPAREN; t = reftype RPAREN; e = expr { TypeCast(t, e) }
  | e1 = expr; o = assignop; e2 = expr { Mutate (e1, o, e2) }

%inline binop:
  | EQU { Equal }
  | NEQ { NotEqual }
  | LESS { Less }
  | GREATER { Greater }
  | LEQ { LessEqual }
  | GEQ { GreaterEqual }
  | AND { And }
  | OR { Or }
  | PLUS { Plus }
  | MINUS { Minus }

%inline assignop:
  | ASSIGN { Regular }
  | PLUS ASSIGN { Incr }
  | MINUS ASSIGN { Decr }

atom:
  | s=IGNORE { Ignore s }
  | i=INT { Int i }
  | f=FLOAT { Float f }
  | s=STRING { String s }
  | s=TYPEREF { TypeRef s }
  | s=SELECTOR { Selector s }
  | s=IDENT { Var s }
  | SELF { Self }
  | NULL { Null }
  | NIL { Nil }
  | YES { Bool true }
  | NO { Bool false }
