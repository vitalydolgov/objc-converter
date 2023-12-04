{
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let upper_alpha = ['A'-'Z']

let int = '-'? digit+
let float = '-'? digit+ '.' digit+

let constant = (upper_alpha | '_')+
let ident = (alpha | '_') (alpha | digit | '_')*
let type_ident = upper_alpha (alpha | digit | '_')*

let whitespace = [' ' '\t']+
let newline = '\n'
let comment = "//" [' ' '\t']* ([^ '\n']* as com)
let string = "@\"" ([^ '"']* as str) "\""
let ignore = '#' ([^ '#']+ as str) '#'

(* Rules *)

rule read = parse
  | whitespace { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | comment { COMMENT (com) }

  | ignore { IGNORE (str) }
  | int { INT (Lexing.lexeme lexbuf |> int_of_string) }
  | float { FLOAT (Lexing.lexeme lexbuf |> float_of_string) }
  | string { STRING (str) }

  | "NULL" { NULL }
  | "NO" { NO }
  | "YES" { YES }

  | ":" { COLON }
  | ";" { SEMICOLON }
  | "." { DOT }
  | "," { COMMA }
  | "*" { ASTERISK }
  | "-" { MINUS }
  | "=" { ASSIGN }
  | "^" { CARET }

  (* Logic *)
  | "&&" { AND }
  | "||" { OR }
  | "==" { EQU }
  | "!" { NOT }
  | "!=" { NEQ }
  | "<" { LESS }
  | ">" { GREATER }
  | "<=" { LEQ }
  | ">=" { GEQ }

  (* Parentheses *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBLOCK }
  | "}" { RBLOCK }
  | "[" { LBRACK }
  | "]" { RBRACK }

  | "self" { SELF }
  | "nil" { NIL }
  | "class" { CLASS }

  | "void" { VOID }
  | "id" { ID }

  | "return" { RETURN }

  | "@selector(" (ident as s) ")" { SELECTOR(s) }

  (* Control flow *)
  | "if" { IF }
  | "else" { ELSE }

  | constant { CONSTANT (Lexing.lexeme lexbuf) }
  | type_ident { TYPE_IDENT (Lexing.lexeme lexbuf) }
  | ident { IDENT (Lexing.lexeme lexbuf) }

  | eof { EOF }
  | _ { failwith ("Unknown character: '" ^ Lexing.lexeme lexbuf ^ "'") }