{
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let float = '-'? digit+ '.' digit+

let identifier = (alpha | '_') (alpha | digit | '_')*

let whitespace = [' ' '\t']+
let newline = '\n'
let comment = "//" [' ' '\t']* ([^ '\n']* as com)

(* Rules *)

rule read = parse
  | whitespace { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | comment { COMMENT (com) }

  | int { INT (Lexing.lexeme lexbuf |> int_of_string) }
  | float { FLOAT (Lexing.lexeme lexbuf |> float_of_string) }

  | "NULL" { NULL }

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
  | "<" { LES }
  | ">" { GRT }
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

  | "if" { IF }
  | "else" { ELSE }
  
  | identifier { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { failwith ("Unknown character: '" ^ Lexing.lexeme lexbuf ^ "'") }