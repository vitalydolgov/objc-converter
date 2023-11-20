{
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let float = '-'? digit+ '.' digit+

let identifier = alpha (alpha | digit)*

let whitespace = [' ' '\t' '\n']+

(* Rules *)

rule read = parse
  | whitespace { read lexbuf }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | int { INT (Lexing.lexeme lexbuf |> int_of_string) }
  | float { FLOAT (Lexing.lexeme lexbuf |> float_of_string) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBLOCK }
  | "}" { RBLOCK }
  | ":" { COLON }
  | "*" { ASTERISK }
  | "-" { MINUS }
  | eof { EOF }
  | _ { failwith ("Unknown character: '" ^ Lexing.lexeme lexbuf ^ "'") }