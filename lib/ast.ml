(** Basic component of an expression. *)
type atom =
  | Int of int
  | Float of float
  | Var of string
  | Prop of string
  | Self
  | Null
  | Nil

(** Binary operator. *)
type binop =
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
[@@deriving show { with_path = false }]

(** Unary operator. *)
type unary =
  | Not
[@@deriving show { with_path = false }]

(** Expression is something that returns a result. *)
type expr =
  | Expr of expr
  | Binary of binop * expr * expr
  | Unary of unary * expr
  | Atom of atom
  | Message of expr * string * (string * expr) list
  (* return type, parameters with types, body *)
  | Block of string option * (string * string) list * statement list

(** Statement is something that returns no result. *)
and statement =
  | If of expr * statement list
  | Else of [`NoCond of statement list | `Cond of statement]
  | NewVar of string * string * expr
  | Mutate of string * expr
  | Comment of string
  | Exec of expr

(** Declaration that compose a program. *)
type declar =
  | Method of { ident : string;
                (* label, type, name *)
                args : (string * string * string) list;
                return_type : string;
                body : statement list }

type program = Program of declar list

(* Helper types *)

type method_comp =
  | Label of string
  | Type of string
  | Identifier of string
  | Return_type of string
  | Body of statement list

let find_last_preposition ident =
  let prepositions = ["For"; "With"; "From"; "In"; "At"] in
  let max_index = String.length ident - 1 in
  let inner prep =
    let rex =
      Printf.sprintf "%s[A-Z][A-Za-z0-9]*$\\|%s$" prep prep
      |> Str.regexp
    in
    try
      let index = Str.search_backward rex ident max_index in
      Some (index, prep)
    with
    | Not_found -> None
  in
  let found_sorted =
    List.filter_map inner prepositions
    |> List.filter (fun (index, _) -> index >= 0)
    |> List.sort (fun (ia, _) (ib, _) -> Int.compare ib ia)
  in
  List.nth_opt found_sorted 0 |> Option.map snd

let split_name_label ident =
  let prep = find_last_preposition ident in
  Option.bind prep (fun prep ->
      let rex =
        Printf.sprintf "\\([A-Za-z0-9_]+\\)\\(%s[A-Za-z0-9]*\\)$" prep
        |> Str.regexp
      in
      try
        let _ = Str.search_forward rex ident 0 in
        let name = Str.matched_group 1 ident in
        let label = Str.matched_group 2 ident |> String.uncapitalize_ascii in
        Some (name, label)
      with
      | Not_found -> None)

(* Produces a method invocation from receiver and message arguments. *)
let make_message recv args =
  let labels, exprs = List.split args in
  let name, label =
    let ident = List.hd labels in
    match split_name_label ident with
    | Some (name, label) -> name, label
    | None -> ident, "_"
  in
  let new_labels = (label :: List.tl labels) in
  let new_args = List.combine new_labels exprs in
  Message (recv, name, new_args)

(* Produces a method from components and statements (its body). *)
let make_method comps body =
  let labels =
    List.filter (function Label _ -> true | _ -> false) comps
    |> List.map (function Label s -> s | _ -> assert false)
  in
  let types =
    List.filter (function Type _ -> true | _ -> false) comps
    |> List.map (function Type s -> s | _ -> assert false)
  in
  let identifiers =
    List.filter (function Identifier _ -> true | _ -> false) comps
    |> List.map (function Identifier s -> s | _ -> assert false)
  in
  let return_type =
    let [@warning "-8"] Return_type typ =
      List.find (function Return_type _ -> true | _ -> false) comps
    in
    typ
  in
  let name, label =
    let ident = List.hd labels in
    match split_name_label ident with
    | Some (name, label) -> name, label
    | None -> ident, "_"
  in
  let args =
    List.mapi
      (fun i label ->
        (label, List.nth types i, List.nth identifiers i))
      (label :: List.tl labels)
  in
  Method { ident = name;
           args = args;
           return_type = return_type;
           body = body }

(* Debug *)

let string_of_list to_string lis =
  "[" ^ (String.concat "; " (List.map to_string lis)) ^ "]"

let wrap_in_parens str =
  if String.contains str ' ' then
    "(" ^ str ^ ")"
  else
    str

let dump_atom = function
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Var s -> "Var " ^ s
  | Prop s -> "Prop " ^ s
  | Self -> "Self"
  | Null -> "NULL"
  | Nil -> "Nil"

let rec dump_expr = function
  | Atom a -> dump_atom a
  | Expr e -> dump_expr e
  | Binary (op, e1, e2) -> dump_binop_expr op e1 e2
  | Unary (op, e) -> dump_unary_expr op e
  | Message (expr, name, args) ->
     let string_of_args =
       string_of_list (fun (s, e) -> s ^ ": " ^ (dump_expr e))
     in
     Printf.sprintf "Message %s . %s %s"
       (dump_expr expr |> wrap_in_parens) name
       (string_of_args args)
  | Block (ret_type, params, body) ->
     let string_of_param (typ, name) =
       Printf.sprintf "Type %s Name %s" typ name
     in
     Printf.sprintf "Block Return_type %s Params: %s Body: %s"
       (match ret_type with Some t -> t | None -> "void")
       (string_of_list string_of_param params)
       (string_of_list dump_statement body)

and dump_binop_expr op e1 e2 =
  Printf.sprintf "(%s %s %s)"
    (show_binop op)
    (dump_expr e1 |> wrap_in_parens)
    (dump_expr e2 |> wrap_in_parens)

and dump_unary_expr op e =
  Printf.sprintf "(%s %s)"
    (show_unary op)
    (dump_expr e |> wrap_in_parens)

and dump_statement = function
  | If (e, l) ->
     Printf.sprintf "If %s %s"
       (dump_expr e)
       (string_of_list dump_statement l)
  | Else (`NoCond l) ->
     "Else " ^ (string_of_list dump_statement l)
  | Else (`Cond s) ->
     "Else " ^ (dump_statement s)
  | NewVar (t, s, e) ->
     Printf.sprintf "NewVar %s %s := %s"
       t s (dump_expr e)
  | Mutate (s, e) ->
     Printf.sprintf "Mutate %s := %s"
       s (dump_expr e)
  | Comment s -> "Comment // " ^ s
  | Exec e -> dump_expr e

(* Declarations *)

let dump_method_comp = function
  | Label s -> "Label " ^ s
  | Type s -> "Type " ^ s
  | Identifier s -> "Identifier " ^ s
  | Return_type s -> "Return_type " ^ s
  | Body l ->
     "Body " ^ string_of_list dump_statement l

let dump_method_arg (label, typ, ident) =
  Printf.sprintf
    "Label %s Type %s Identifier %s"
    label typ ident

let dump_declar = function
  | Method declar ->
     Printf.sprintf
       "Method Return_type %s Identifier %s Arguments: %s Body: %s"
       declar.return_type
       declar.ident
       (string_of_list dump_method_arg declar.args)
       (string_of_list (fun s -> s |> dump_statement |> wrap_in_parens) declar.body)

let dump_program (Program declars) =
  string_of_list dump_declar declars
