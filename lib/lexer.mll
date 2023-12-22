{
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let float = ('-'? digit+ '.' digit* as value) 'f'?

let ident = (alpha | '_') (alpha | digit | '_')*
let sel_ident = (alpha | '_') (alpha | digit | '_' | ':')*

let whitespace = [' ' '\t']+
let newline = '\n'
let comment = "//" [' ' '\t']* ([^ '\n']* as com)
let string = "@\"" ([^ '"']* as str) "\""
let mark = "#pragma mark " ([^ '\n']* as com)

let implem_start =
    "@implementation" whitespace+ ident+ (whitespace+ '(' ident* ')')?
let implem_end = "@end"

let dynamic = "@dynamic" [^ ';']* ';'
let synthesize = "@synthesize" [^ ';']* ';'

(* Rules *)

rule read = parse
  | whitespace { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | "!{" { TEST_START }

  | synthesize | dynamic { read lexbuf }
  | comment { COMMENT (com) }
  | mark { MARK (com) }
  | implem_start { IMPLEM_START }
  | implem_end { IMPLEM_END }

  | "_Nonnull" { NONNULL }
  | "_Nullable" { NULLABLE }
  | "__weak" { WEAK }

  | int { INT (Lexing.lexeme lexbuf |> int_of_string) }
  | float { FLOAT (value |> float_of_string) }
  | string { STRING (str) }

  | "NULL" { NULL }
  | "NO"
  | "@NO" { NO }
  | "YES"
  | "@YES" { YES }
  | "@" (digit+ as value) { INT(value |> int_of_string) }

  | "=" { ASSIGN }

  | ":" { COLON }
  | ";" { SEMICOLON }
  | "." { DOT }
  | "," { COMMA }
  | "^" { CARET }
  | "@" { AT }
  | "?" { QUESTION }

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

  | "-" { MINUS }
  | "+" { PLUS }
  | "*" { ASTERISK }
  | "/" { SLASH }

  (* Parentheses *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBLOCK }
  | "}" { RBLOCK }
  | "[" { LBRACK }
  | "]" { RBRACK }

  | "id" { ID }
  | "self" { SELF }
  | "class" { CLASS }
  | "nil" { NIL }
  | (ident as s) ".class"
  | (ident as s) ".self" { TYPEREF (s) }
  | (ident as g) whitespace* "<" (ident as s) whitespace? '*' ">" { GENTYPE (g, s) }
  | "id"  whitespace* "<" (ident as p) ">" { IDPROTO (p) }
  | (ident as t) whitespace* "<" (ident as p) ">" { TYPEPROTO (t, p) }

  | "return" { RETURN }

  | "@selector(" (sel_ident as s) ")" { SELECTOR(s) }

  (* Control flow *)
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "in" { IN }
  | "while" { WHILE }
  | "do" { DO }

  | ident { IDENT (Lexing.lexeme lexbuf) }
  
  | eof { EOF }
  | _ { failwith ("Unknown character: '" ^ Lexing.lexeme lexbuf ^ "'") }