open Ast

module StringSet = Set.Make (String)

let legacy_types =
  [ "NSIndexPath";
    "NSString" ]
  |> StringSet.of_list

let drop_prefix typ =
  let len = String.length typ - 2 in
  String.sub (StringSet.find typ legacy_types) 2 len

let map_type typ =
  try drop_prefix typ with
  | Not_found -> typ

let indent_region str =
  String.split_on_char '\n' str
  |> List.map (fun line ->
         String.make 4 ' ' ^ line)
  |> String.concat "\n"

let convert_decl_args args =
  if args = [] then ""
  else
    let (label, _, _) = List.hd args in
    let converted_args =
      args
      |> List.map (fun (_, t, n) -> n ^ ": " ^ map_type t)
      |> String.concat ", "
    in
    String.concat " " [label; converted_args]

let convert_invoc_args to_string args =
  if args = [] then ""
  else
    let (label, _) = List.hd args in
    let converted_args =
      args
      |> List.map (fun pair -> snd pair |> to_string)
      |> String.concat ", "
    in
    if label = "_" then
      Printf.sprintf "%s" converted_args
    else
      Printf.sprintf "%s: %s" label converted_args

let convert_block_args args = match args with
  | [] -> ""
  | [(_, name)] -> name
  | l -> "(" ^ (List.map snd l |> String.concat ", ") ^ ")"

let rec convert_statement = function
  | If (expr, body) ->
     Printf.sprintf "if %s {\n%s\n}"
       (convert_expr expr)
       (convert_body_indented body)
  | Else (`NoCond body) ->
     Printf.sprintf "else {\n%s\n}"
       (convert_body_indented body)
  | Else (`Cond stat) ->
     Printf.sprintf "else %s"
       (convert_statement stat)
  | NewVar (_, name, expr) ->
     Printf.sprintf "var %s = %s"
       name (convert_expr expr)
  | Mutate (name, expr) ->
     Printf.sprintf "%s = %s"
       name (convert_expr expr)
  | Comment comm -> "// " ^ comm
  | Exec expr -> convert_expr expr

and convert_expr = function
  | Expr expr -> "(" ^ convert_expr expr ^ ")"
  | Binary (op, expr1, expr2) ->
     Printf.sprintf "%s %s %s"
       (convert_expr expr1)
       (convert_binop op)
       (convert_expr expr2)
  | Unary (op, expr) ->
     Printf.sprintf "%s%s"
       (convert_unary op)
       (convert_expr expr)
  | Atom atom -> convert_atom atom
  | Message (Atom Self, ident, args) ->
     Printf.sprintf "%s(%s)"
       ident
       (convert_invoc_args (convert_expr) args)
  | Message (expr, ident, args) ->
     Printf.sprintf "%s.%s(%s)"
       (convert_expr expr)
       ident
       (convert_invoc_args (convert_expr) args)
  | Block (_, [], body) ->
     Printf.sprintf "{\n%s\n}"
       (convert_body_indented body)
  | Block (_, args, body) ->
     Printf.sprintf "{ %s in\n%s\n}"
       (convert_block_args args)
       (convert_body_indented body)

and convert_binop = function
  | And -> "&&"
  | Or -> "||"
  | Equal -> "=="
  | NotEqual -> "!="
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="

and convert_unary = function
  | Not -> "!"

and convert_atom = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Var x | Prop x -> x
  | Self -> "self"
  | Nil -> "nil"
  | Null -> assert false

and convert_body_indented body =
  List.map convert_statement body |> String.concat "\n" |> indent_region

let convert_return_type typ =
  if typ = "void" then ""
  else " -> " ^ map_type typ

let convert_declar = function
  | Method { ident; args; return_type; body } ->
     Printf.sprintf "func %s(%s)%s {\n%s\n}"
       ident
       (convert_decl_args args)
       (convert_return_type return_type)
       (convert_body_indented body)

let process (Program program) =
  let declars = List.map convert_declar program in
  String.concat "\n" declars
