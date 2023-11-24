open Objc_converter.Facade

let read_file filename =
  let ch = open_in_bin filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str

let _ =
  let file = Array.get Sys.argv 1 in
  let text = read_file file in
  let output = process text in
  print_endline output
