open Ast

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let legacy_types =
  [ "NSIndexPath";
    "NSString" ]
  |> StringSet.of_list

let drop_prefix typ =
  let len = String.length typ - 2 in
  String.sub (StringSet.find typ legacy_types) 2 len

let objc_to_swift_types =
  let open StringMap in
  empty
  |> add "BOOL" "Bool"

let map_type = function
  | Simple objc_type ->
     begin
       try drop_prefix objc_type with
       | Not_found ->
          match StringMap.find_opt objc_type objc_to_swift_types with
          | Some swift_type -> swift_type
          | None -> objc_type
     end
  | Generic (g, t) -> Printf.sprintf "%s<%s>" g t
  | Void -> "Void"
  | Any -> "Any"

let indent_region str =
  String.split_on_char '\n' str
  |> List.map (fun line ->
         String.make 4 ' ' ^ line)
  |> String.concat "\n"

let convert_params args =
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
     Printf.sprintf "let %s = %s"
       name (convert_expr expr)
  | Mutate (expr1, expr2) ->
     Printf.sprintf "%s = %s"
       (convert_expr expr1)
       (convert_expr expr2)
  | Comment comm -> "// " ^ comm
  | Exec expr -> convert_expr expr
  | Return None -> "return"
  | Return (Some expr) -> "return " ^ (convert_expr expr)
  | For (_, ident, expr, body) ->
     Printf.sprintf "for %s in %s {\n%s\n}"
       ident (convert_expr expr)
       (convert_body_indented body)

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
       ident (convert_invoc_args (convert_expr) args)
  | Message (expr, "alloc", []) ->
     Printf.sprintf "%s" (convert_expr expr)
  | Message (expr1, "isEqualToString", [("_", expr2)]) ->
     Printf.sprintf "%s == %s"
       (convert_expr expr1)
       (convert_expr expr2)
  | Message (expr, ident, args) ->
     Printf.sprintf "%s.%s(%s)"
       (convert_expr expr) ident
       (convert_invoc_args (convert_expr) args)
  | Property (Atom Self, ident) -> ident
  | Property (expr, ident) ->
     Printf.sprintf "%s.%s" (convert_expr expr) ident
  | Block (_, [], body) ->
     Printf.sprintf "{\n%s\n}" (convert_body_indented body)
  | Block (_, args, body) ->
     Printf.sprintf "{ %s in\n%s\n}"
       (convert_block_args args)
       (convert_body_indented body)
  | TypeCast (typ, expr) ->
     Printf.sprintf "(%s as! %s)"
       (convert_expr expr)
       (map_type typ)
  | Element (expr1, expr2) ->
     Printf.sprintf "%s[%s]"
       (convert_expr expr1)
       (convert_expr expr2)
  | Func (ident, args) ->
     Printf.sprintf "%s(%s)" ident
       (String.concat ", "(List.map convert_expr args))
  | Array atoms ->
     Printf.sprintf "[%s]"
       (String.concat ", "(List.map convert_atom atoms))

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
  | Ignore s -> "#" ^ s ^ "#"
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Var x | Prop x -> x
  | TypeRef s -> s ^ ".self"
  | Self -> "self"
  | Nil | Null -> "nil"
  | Type t -> map_type t
  | Selector s -> "Selector(" ^ s ^ ")"

and convert_body_indented body =
  List.map convert_statement body |> String.concat "\n" |> indent_region

let convert_return_type typ =
  if typ = Void then ""
  else " -> " ^ map_type typ

let convert_declar = function
  | Method { ident; params; return_type; body } ->
     Printf.sprintf "@objc public func %s(%s)%s {\n%s\n}"
       ident
       (convert_params params)
       (convert_return_type return_type)
       (convert_body_indented body)

let process (Program program) =
  let declars = List.map convert_declar program in
  String.concat "\n" declars
