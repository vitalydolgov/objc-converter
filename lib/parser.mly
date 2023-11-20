%{
    open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER
%token LPAREN
%token RPAREN
%token COLON
%token ASTERISK
%token MINUS
%token LBLOCK
%token RBLOCK

%token EOF

/* Types */

%type <Ast.program> program

%start program

%%

program:
  | declar* EOF { Program $1 }
;

declar:
  | m = method_comp*; LBLOCK; b = statement*; RBLOCK
    { make_method m b }

method_comp:
  | MINUS; LPAREN; s = IDENTIFIER; ASTERISK?; RPAREN { Return_type s }
  | LPAREN; s = IDENTIFIER; ASTERISK?; RPAREN { Type s }
  | s = IDENTIFIER; COLON { Label s }
  | s = IDENTIFIER { Identifier s }

statement:
  | s = IDENTIFIER { Dummy }
  | i = INT { Int i }
