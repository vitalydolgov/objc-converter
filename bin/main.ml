open Lib.Ast
module Parser = Lib.Parser
module Lexer = Lib.Lexer

open Lexing

let read_file filename =
  let ch = open_in_bin filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str

let colnum pos =
  pos.pos_cnum - pos.pos_bol - 1

let string_of_pos pos =
  let l = pos.pos_lnum in
  let c = colnum pos + 1 in
  Printf.sprintf "%i:%i" l c

let parse_program str =
  let lexbuf = Lexing.from_string str in
  try
    Parser.program Lexer.read lexbuf
  with Parser.Error ->
    let mesg = "Parse error at " ^ (string_of_pos lexbuf.lex_curr_p) in
    failwith mesg

let _ =
  let file = Array.get Sys.argv 1 in
  let text = read_file file in
  let (Program program) = parse_program text in
  print_string @@ dump_program (Program program)
