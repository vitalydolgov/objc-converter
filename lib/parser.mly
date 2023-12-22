%{
    open Ast
%}

(* Literals *)

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token YES NO

%token SELF
%token CLASS
%token NULL NIL

%token <string> IDPROTO
%token <string * string> TYPEPROTO

%token <string> COMMENT
%token <string> MARK

%token COLON
%token SEMICOLON
%token DOT
%token COMMA
%token CARET
%token AT
%token QUESTION (* default, ternary *)

%token MINUS
%token PLUS
%token ASTERISK (* pointer, multiplication *)
%token SLASH (* division *)

%token ASSIGN

%token NONNULL NULLABLE
%token WEAK

(* Parentheses *)

%token LPAREN RPAREN
%token LBLOCK RBLOCK
%token LBRACK RBRACK

(* Keywords *)

%token IF ELSE
%token FOR IN
%token WHILE DO
%token RETURN

(* Operators *)

%token AND OR

%token NOT
%token EQU NEQ LEQ GEQ
%token LESS GREATER

%token ID

%token <string> IDENT
%token <string> TYPEREF
%token <string> SELECTOR

%token IMPLEM_START IMPLEM_END
%token TEST_START

%token EOF

%left ASSIGN
%right IDENT
%left PLUS MINUS
%left ASTERISK SLASH
%left OR
%right NOT
%left LESS GREATER (* comparison, generics *)
%left LEQ GEQ EQU NEQ
%left DOT
%left AND
%left QUESTION COLON
%right LBRACK
%right RPAREN

%type <Ast.program> program

%start program

%%

program:
  | TEST_START; e = expr RBLOCK EOF { Statement (Exec e) }
  | TEST_START; s = statement RBLOCK EOF { Statement s }
  | IMPLEM_START; p = declar*; IMPLEM_END EOF { Program p }
  | declar* EOF { Program $1 }
;

comment:
  | s=MARK { Mark s }
  | s=COMMENT { LineComment s }

declar:
  | c = comment { Comment (c) }
  | m = method_comp* LBLOCK; b = statement* RBLOCK
    { make_method m b }

method_comp:
  | MINUS LPAREN t = typ RPAREN { Return_type (false, t) }
  | PLUS LPAREN t = typ RPAREN { Return_type (true, t) }
  | LPAREN; t = typ RPAREN s = IDENT { Param (t, s) }
  | s=IDENT COLON { Label s }
  | s=IDENT { Identifier s }

statement:
  | IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK { If (e, b) }
  | IF LPAREN; e = expr RPAREN s = statement { If (e, [s]) }
  | ELSE IF LPAREN; e = expr RPAREN LBLOCK; b = statement* RBLOCK
    { Else (`Cond (If (e, b))) }
  | ELSE LBLOCK; b = statement* RBLOCK { Else (`NoCond b) }
  | s=COMMENT { Comment s }
  | e = expr SEMICOLON { Exec e }
  | RETURN; e = expr? SEMICOLON { Return e }
  | x = declaration { x }
  | x = loop { x }

declaration:
  | weak_opt = ioption(WEAK { () }); t = typ s=IDENT expr_opt = ioption(ASSIGN e = expr { e }) SEMICOLON
    { let ownership = match weak_opt with
	| Some _ -> Weak
	| None -> match t with
		  | PrimitiveType _ -> NoOwnership
      		  | _ -> Strong
      in
      let expr = match expr_opt with
	| Some e -> e
	| None -> Atom NoValue
      in
      NewVar (ownership, t, s, expr) }

loop:
  | FOR LPAREN; t = typ x=IDENT IN; e = expr RPAREN LBLOCK b = statement* RBLOCK
    { ForEach (t, x, e, b) }
  | FOR LPAREN; s = declaration SEMICOLON e1 = expr; SEMICOLON e2 = expr RPAREN LBLOCK b = statement* RBLOCK
    { For (s, e1, e2, b) }
  | WHILE LPAREN; e = expr RPAREN LBLOCK b = statement* RBLOCK { While (e, b) }
  | DO LBLOCK b = statement* RBLOCK WHILE LPAREN; e = expr RPAREN SEMICOLON { Repeat (b, e) }

typ:
  | t = reftype { t }
  | s=IDENT { make_primitive_type s }

reftype:
  | ID { make_type "id" }
  | s=IDENT ASTERISK NONNULL { make_type s }
  | s=IDENT ASTERISK NULLABLE? { Optional (make_type s) }
  | p=IDPROTO NONNULL? { make_protocol_type "id" p }
  | p=TYPEPROTO ASTERISK NONNULL { make_protocol_type (fst p) (snd p) }
  | p=TYPEPROTO ASTERISK NULLABLE? { Optional (make_protocol_type (fst p) (snd p)) }
  | s=IDENT LESS; l = separated_list(COMMA, t = reftype { t }) GREATER ASTERISK NULLABLE?
    { Optional (make_generic_type s l) }

expr:
  | AT LPAREN; e = expr RPAREN
  | LPAREN; e = expr RPAREN { Expr e }
  | LBRACK; e = expr ID RBRACK | e = expr DOT ID { Property (e, Ident "id") }
  | LBRACK; s=IDENT CLASS RBRACK
  | LBRACK; s=IDENT SELF RBRACK { Atom (TypeRef (ObjectType s)) }
  | LBRACK; e = expr s=IDENT RBRACK { Message (e, s, []) }
  | LBRACK; e = expr; l = list(s=IDENT COLON; e = expr { (s, e) }) RBRACK
    { make_message e l }
  | LBRACK; e = expr; l = list(s=IDENT COLON; e = expr { (s, e) }) COMMA va = separated_list(COMMA, e = expr { e }) RBRACK
    { make_message_vararg e l ("_", va) }
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
  | e = expr PLUS PLUS
  | PLUS PLUS; e = expr { Assign (IncAssign, e, Atom (Literal (Int 1))) }
  | e = expr MINUS MINUS
  | MINUS MINUS; e = expr  { Assign (DecAssign, e, Atom (Literal (Int 1))) }
  | e = expr; DOT x = ident { Property (e, x) }
  | e1 = expr LBRACK; e2 = expr RBRACK { Element(e1, e2) }
  | s=IDENT LPAREN; l = separated_list(COMMA, e = expr { e }) RPAREN { Func(s, l) }
  | e = expr; DOT s=IDENT LPAREN; l = separated_list(COMMA, e = expr { ("_", NormalArg e) }) RPAREN { Message(e, s, l) }
  | AT LBRACK l = separated_list(COMMA, a = atom { a }) RBRACK { ArrayValues l }
  | AT LBLOCK l = separated_list(COMMA, k = atom COLON v = atom { (k, v) }) RBLOCK { DictionaryValues l }
  | a = atom { Atom a }
  | LPAREN; t = reftype RPAREN; e = expr
    { match t with
      | Optional t' -> TypeCast(t', e)
      | _ -> TypeCast(t, e) }
  | e1 = expr; o = assignop; e2 = expr { Assign (o, e1, e2) }
  | c = expr QUESTION; e1 = expr COLON; e2 = expr { Ternary (c, e1, e2) }

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
  | ASTERISK { Times }
  | SLASH { Divide }
  | QUESTION COLON { Default }

%inline assignop:
  | ASSIGN { ValAssign }
  | PLUS ASSIGN { IncAssign }
  | MINUS ASSIGN { DecAssign }

atom:
  | l = literal { Literal l }
  | x = ident { x }
  | s=TYPEREF { TypeRef (ObjectType s) }
  | s=SELECTOR { Selector s }
  | SELF { Self }
  | NULL | NIL { NoValue }

literal:
  | i=INT { Int i }
  | MINUS i=INT { Int (Int.neg i) }
  | f=FLOAT { Float f }
  | MINUS f=FLOAT { Float (Float.neg f) }
  | s=STRING { String s }
  | YES { Bool true }
  | NO { Bool false }

ident:
 | s=IDENT { Ident s }
