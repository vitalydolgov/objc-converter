open Ast

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let placeholder str = "[<#" ^ str ^ "#>]"

let legacy_types =
  [ "NSIndexPath";
    "NSString";
    "NSDateFormatter" ]
  |> StringSet.of_list

let drop_prefix typ =
  let len = String.length typ - 2 in
  String.sub (StringSet.find typ legacy_types) 2 len

let objc_to_swift_types =
  let open StringMap in
  empty
  |> add "BOOL" "Bool"
  |> add "NSInteger" "Int"
  |> add "NSUInteger" "Int"
  |> add "NSArray" (placeholder "type")

let rec map_type = function
  | Simple objc_type ->
     begin
       try drop_prefix objc_type with
       | Not_found ->
          match StringMap.find_opt objc_type objc_to_swift_types with
          | Some swift_type -> swift_type
          | None -> objc_type
     end
  | Generic (g, t) -> Printf.sprintf "%s<%s>" (map_type g) (map_type t)
  | Void -> "Void"
  | Any -> "Any"
  | InstanceType -> "Self.Type"
  | Array t -> Printf.sprintf "[%s]" (map_type t)
  | Optional (Protocoled (t, p)) ->
     Printf.sprintf "(%s)?" (map_type (Protocoled (t, p)))
  | Optional t -> (map_type t) ^ "?"
  | Protocoled (Any, p) -> p
  | Protocoled (t, p) -> (map_type t) ^ " & " ^ p

let indent_region str =
  String.split_on_char '\n' str
  |> List.map (fun line ->
         String.make 4 ' ' ^ line)
  |> String.concat "\n"

let convert_params args =
  if args = [] then ""
  else
    let (label, _, name) = List.hd args in
    let converted_args =
      args
      |> List.map (fun (_, t, n) -> n ^ ": " ^ map_type t)
      |> String.concat ", "
    in
    begin
      if label = name then
        [converted_args]
      else
        [label; converted_args]
    end
    |> String.concat " "

let convert_invoc_args to_string args =
  let converted_args =
    let inner (label, value) =
      let value =
        match value with
        | NormalArg expr -> to_string expr
        | VarArg lis -> String.concat ", " (List.map (fun expr -> to_string expr) lis)
      in
      match label with
      | "_" -> value
      | _ -> label ^ ": " ^ value
    in
    args
    |> List.map inner
    |> String.concat ", "
  in
  Printf.sprintf "%s" converted_args

let convert_block_args args = match args with
  | [] -> ""
  | [(_, name)] -> name
  | l -> "(" ^ (List.map snd l |> String.concat ", ") ^ ")"

let convert_assign = function
  | Regular -> "="
  | Incr -> "+="
  | Decr -> "-="

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
  | Comment comm -> "// " ^ comm
  | Exec expr -> convert_expr expr
  | Return None -> "return"
  | Return (Some expr) -> "return " ^ (convert_expr expr)
  | ForEach (_, ident, expr, body) ->
     Printf.sprintf "for %s in %s {\n%s\n}"
       ident (convert_expr expr)
       (convert_body_indented body)
  | For (assign, cond, incr, body) ->
     let [@warning "-8"] NewVar (_, ident, init) = assign in
     Printf.sprintf "var %s = %s\nwhile %s {\n%s\n}"
       ident (convert_expr init)
       (convert_expr cond)
       (convert_body_indented (body @ [Exec incr]))
  | While (cond, body) ->
     Printf.sprintf "while %s {\n%s\n}"
       (convert_expr cond)
       (convert_body_indented body)
  | Repeat (body, cond) ->
     Printf.sprintf "repeat {\n%s\n} while %s"
       (convert_body_indented body)
       (convert_expr cond)

and convert_expr = function
  | Expr expr -> "(" ^ convert_expr expr ^ ")"
  | Binary (op, expr1, expr2) ->
     Printf.sprintf "%s %s %s"
       (convert_expr expr1)
       (convert_binop op)
       (convert_expr expr2)
  | Unary (Not, expr) ->
     Printf.sprintf "!%s"
       (convert_expr expr)
  | Atom atom -> convert_atom atom
  | Message _ as m -> convert_message m
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
  | ArrayValues atoms ->
     Printf.sprintf "[%s]"
       (String.concat ", "(List.map convert_atom atoms))
  | Mutate (expr1, assign, expr2) ->
     Printf.sprintf "%s %s %s"
       (convert_expr expr1)
       (convert_assign assign)
       (convert_expr expr2)
  | Ternary (cond, expr1, expr2) ->
     Printf.sprintf "%s ? %s : %s"
       (convert_expr cond)
       (convert_expr expr1)
       (convert_expr expr2)

and convert_message mesg = match [@warning "-8"] mesg with
  | Message (Atom Self, ident, args) ->
     Printf.sprintf "%s(%s)"
       ident (convert_invoc_args (convert_expr) args)
  | Message (expr, "alloc", []) ->
     Printf.sprintf "%s" (convert_expr expr)
  | Message (expr, "new", []) ->
     Printf.sprintf "%s()" (convert_expr expr)
  | Message (expr, "init", args) ->
     Printf.sprintf "%s(%s)"
       (convert_expr expr)
       (convert_invoc_args (convert_expr) args)
  | Message (expr, ident, args) ->
     Printf.sprintf "%s.%s(%s)"
       (convert_expr expr) ident
       (convert_invoc_args (convert_expr_nowrap) args)

and convert_expr_nowrap = function
  | Expr expr -> convert_expr expr
  | expr -> convert_expr expr

and convert_binop = function
  | And -> "&&"
  | Or -> "||"
  | Equal -> "=="
  | NotEqual -> "!="
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Default -> "??"

and convert_atom = function
  | Ignore s -> "~ignored: " ^ s ^ "~"
  | Int i -> string_of_int i
  | Float f ->
     if Float.is_integer f then
       (f |> int_of_float |> string_of_int) ^ ".0"
     else string_of_float f
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Var x | Prop x -> x
  | TypeRef s -> s ^ ".self"
  | Self -> "self"
  | Nil | Null -> "nil"
  | Type t -> map_type t
  | Selector s -> "Selector(\"" ^ s ^ "\")"

and convert_body_indented body =
  List.map convert_statement body |> String.concat "\n" |> indent_region

let convert_return_type typ =
  if typ = Void || typ = InstanceType then ""
  else " -> " ^ map_type typ

let convert_declar = function
  | Method { is_static; ident; params; return_type; body } ->
    let body' = match body with
      | [Return (Some expr)] -> [Exec expr]
      | _ -> body
    in
    Printf.sprintf "@objc public %s%s(%s)%s {\n%s\n}"
      (if is_static then "static " else "")
      (if ident = "init" then "init" else "func " ^ ident)
      (convert_params params)
      (convert_return_type return_type)
      (convert_body_indented body')
  | Comment (Mark str) -> "// MARK: " ^ str
  | Comment (LineComment str) -> "// " ^ str

let process (Program program) =
  let declars = List.map convert_declar program in
  String.concat "\n" declars
