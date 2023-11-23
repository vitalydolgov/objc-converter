{
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let float = '-'? digit+ '.' digit+

let identifier = alpha (alpha | digit)*

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
  | "*" { ASTERISK }
  | "-" { MINUS }
  | "=" { ASSIGN }

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

  | "if" { IF }
  
  | identifier { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { failwith ("Unknown character: '" ^ Lexing.lexeme lexbuf ^ "'") }