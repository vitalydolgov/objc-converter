open Ast
open Lexing

let colnum pos =
  pos.pos_cnum - pos.pos_bol - 1

let string_of_pos pos =
  let l = pos.pos_lnum in
  let c = colnum pos + 1 in
  Printf.sprintf "%i:%i" l c

let parse str =
  let lexbuf = Lexing.from_string str in
  try
    Parser.program Lexer.read lexbuf
  with Parser.Error ->
    let mesg = "Parse error at " ^ (string_of_pos lexbuf.lex_curr_p) in
    failwith mesg

let process str =
  try
    let program = parse str in
    Converter.process program
  with
  | Failure mesg -> mesg

let dump str =
  try
    let program = parse str in
    dump_program program
  with
  | Failure mesg -> mesg
