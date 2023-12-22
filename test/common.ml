open Objc_converter
open Objc_converter.Ast

let ( ~: ) = Printf.sprintf "%s:%s"

let parse str = "!{" ^ str ^ "}" |> Facade.parse

let assert_equal_literal str lit = fun _ ->
  assert_equal (Statement (Exec (Atom (Literal lit)))) (parse str)

let assert_equal_atom str atom = fun _ ->
  assert_equal (Statement (Exec (Atom atom))) (parse str)
