type typ =
  | Simple of string
  | Generic of typ * typ
  | Void
  | Any
  | InstanceType
  | Array of typ
  | Optional of typ
  | Protocoled of typ * string

(** Basic component of an expression. *)
type atom =
  | Ignore of string
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Var of string
  | Prop of string
  | TypeRef of string
  | Type of typ
  | Selector of string
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
  | Plus
  | Minus
  | Times
  | Divide
  | Default
[@@deriving show { with_path = false }]

(** Unary operator. *)
type unary =
  | Not
[@@deriving show { with_path = false }]

(** Assignment operators. *)
type assignop =
  | Regular
  | Incr
  | Decr
[@@deriving show { with_path = false }]

(** Expression is something that returns a result. *)
type expr =
  | Expr of expr
  | Binary of binop * expr * expr
  | Unary of unary * expr
  | Atom of atom
  | Message of expr * string * (string * arg) list
  | Property of expr * string
  (* return type, parameters with types, body *)
  | Block of typ * (typ * string) list * statement list
  | TypeCast of typ * expr
  | Element of expr * expr
  | Func of string * expr list
  | ArrayValues of atom list
  | Mutate of expr * assignop * expr
  | Ternary of expr * expr * expr

(** Statement is something that returns no result. *)
and statement =
  | If of expr * statement list
  | Else of [`NoCond of statement list | `Cond of statement]
  | NewVar of typ * string * expr
  | Comment of string
  | Exec of expr
  | Return of expr option
  | ForEach of typ * string * expr * statement list
  | For of statement * expr * expr * statement list
  | While of expr * statement list
  | Repeat of statement list * expr

and arg =
  | NormalArg of expr
  | VarArg of expr list

type comment =
  | Mark of string
  | LineComment of string

(** Declaration that compose a program. *)
type declar =
  | Method of { is_static : bool;
                ident : string;
                (* label, type, name *)
                params : (string * typ * string) list;
                return_type : typ;
                body : statement list }
  | Comment of comment

type program = Program of declar list

(* Helper types *)

type method_comp =
  | Label of string
  | Param of typ * string
  | Identifier of string
  (* static, type *)
  | Return_type of bool * typ
  | Body of statement list

let find_last_preposition ident =
  let prepositions = ["For"; "With"; "From"; "In"; "At"; "Of"; "To"; "By"] in
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

let drop_prep_label prep ident =
  let rex =
    Printf.sprintf "%s\\([A-Za-z0-9]*\\)" prep
    |> Str.regexp
  in
  let _ = Str.search_forward rex ident 0 in
  let label =
    Str.matched_group 1 ident
    |> String.uncapitalize_ascii
  in
  label

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
        let label =
          begin
            let text = Str.matched_group 2 ident in
            let prep_lower = String.lowercase_ascii prep in
            if prep <> text && prep_lower = "with" then
              drop_prep_label prep text
            else
              prep
          end
          |> String.uncapitalize_ascii
        in
        Some (name, label)
      with
      | Not_found -> None)

(* Produces a method invocation from receiver and message arguments. *)
let make_message recv labels_args =
  let labels, exprs = List.split labels_args in
  let name, label =
    let ident = List.hd labels in
    match split_name_label ident with
    | Some (name, label) -> name, label
    | None -> ident, "_"
  in
  let new_labels = (label :: List.tl labels) in
  let exprs' = List.map (fun expr -> NormalArg expr) exprs in
  let new_args = List.combine new_labels exprs' in
  Message (recv, name, new_args)

let make_message_vararg recv labels_args (va_label, va_list) =
  let labels, exprs = List.split labels_args in
  let name, label =
    let ident = List.hd labels in
    match split_name_label ident with
    | Some (name, label) -> name, label
    | None -> ident, "_"
  in
  let new_labels = (label :: List.tl labels) in
  let exprs' = List.map (fun expr -> NormalArg expr) exprs in
  let new_args = List.combine new_labels exprs' @ [(va_label, VarArg va_list)] in
  Message (recv, name, new_args)

let make_method_wo_params is_static return_type name body =
  Method { is_static = is_static;
           ident = name;
           params = [];
           return_type = return_type;
           body = body }

let make_method_w_params is_static return_type labels params body =
  let name, label =
    let ident = List.hd labels in
    match split_name_label ident with
    | Some (name, label) -> name, label
    | None -> ident, "_"
  in
  let args =
    List.mapi
      (fun i label ->
        let (typ, ident) = List.nth params i in
        (label, typ, ident))
      (label :: List.tl labels)
  in
  Method { is_static = is_static;
           ident = name;
           params = args;
           return_type = return_type;
           body = body }

(* Produces a method from components and statements (its body). *)
let make_method comps body =
  let labels =
    List.filter (function Label _ -> true | _ -> false) comps
    |> List.map (function Label s -> s | _ -> assert false)
  in
  let params =
    List.filter (function Param _ -> true | _ -> false) comps
    |> List.map (function Param (t, s) -> (t, s) | _ -> assert false)
  in
  let identifiers =
    List.filter (function Identifier _ -> true | _ -> false) comps
    |> List.map (function Identifier s -> s | _ -> assert false)
  in
  let is_static, return_type =
    let [@warning "-8"] Return_type (is_static, typ) =
      List.find (function Return_type _ -> true | _ -> false) comps
    in
    is_static, typ
  in
  if labels = [] then
    make_method_wo_params is_static return_type (List.hd identifiers) body
  else
    make_method_w_params is_static return_type labels params body

let make_type = function
  | "void" -> Void
  | "id" -> Any
  | "instancetype" -> InstanceType
  | t -> Simple t

let make_generic_type g t =
  match g with
  | "NSArray" | "NSMutableArray" -> Array (make_type t)
  | _ -> Generic (make_type g, make_type t)

let make_protocol_type t p =
  Protocoled (make_type t, p)

(* Debug *)

let string_of_list to_string lis =
  "[" ^ (String.concat "; " (List.map to_string lis)) ^ "]"

let wrap_in_parens str =
  if String.contains str ' ' then
    "(" ^ str ^ ")"
  else
    str

let rec dump_type = function
  | Generic (g, t) ->
     Printf.sprintf "GenericType %s<%s>" (dump_type g) (dump_type t)
  | Simple t -> "Type " ^ t
  | Void -> "Void"
  | Any -> "Any"
  | InstanceType -> "InstanceType"
  | Array t -> "Array " ^ (dump_type t)
  | Optional t -> "Optional " ^ (dump_type t)
  | Protocoled (t, p) -> (dump_type t) ^ " with " ^ p

let dump_atom = function
  | Ignore s -> "# " ^ s ^ " #"
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Bool b -> "Bool " ^ string_of_bool b
  | String s -> "String " ^ s
  | Var s -> "Var " ^ s
  | Prop s -> "Prop " ^ s
  | TypeRef s -> "Type " ^ s
  | Self -> "Self"
  | Null -> "NULL"
  | Nil -> "Nil"
  | Type t -> dump_type t
  | Selector s -> "Selector " ^ s

let rec dump_expr = function
  | Atom a -> dump_atom a
  | Expr e -> dump_expr e
  | Binary (op, e1, e2) -> dump_binop_expr op e1 e2
  | Unary (op, e) -> dump_unary_expr op e
  | Message (expr, name, args) ->
     let string_of_args =
       string_of_list (fun (label, arg) ->
           let arg_str =
             match arg with
             | NormalArg expr -> (dump_expr expr)
             | VarArg lis -> string_of_list dump_expr lis
           in
           label ^ ": " ^ arg_str)
     in
     Printf.sprintf "Message %s . %s %s"
       (dump_expr expr |> wrap_in_parens) name
       (string_of_args args)
  | Property (expr, ident) ->
     Printf.sprintf "Property %s . %s"
       (dump_expr expr |> wrap_in_parens) ident
  | Block (ret_type, params, body) ->
     let string_of_param (typ, name) =
       Printf.sprintf "%s Name %s" (dump_type typ) name
     in
     Printf.sprintf "Block Return_type %s Params: %s Body: %s"
       (dump_type ret_type)
       (string_of_list string_of_param params)
       (string_of_list dump_statement body)
  | TypeCast (typ, expr) ->
     Printf.sprintf "Cast %s as %s" (dump_expr expr) (dump_type typ)
  | Element (expr1, expr2) ->
     Printf.sprintf "Element %s [ %s ]"
       (dump_expr expr1)
       (dump_expr expr2)
  | Func (ident, exprs) ->
     Printf.sprintf "Function %s ( %s )"
       ident (string_of_list dump_expr exprs)
  | ArrayValues atoms ->
     Printf.sprintf "Array %s" (string_of_list dump_atom atoms)
  | Mutate (e1, op, e2) ->
     Printf.sprintf "Mutate %s %s %s"
       (dump_expr e1) (show_assignop op) (dump_expr e2)
  | Ternary (cond, expr1, expr2) ->
     Printf.sprintf "Ternary %s ? %s : %s"
       (dump_expr cond)
       (dump_expr expr1)
       (dump_expr expr2)

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
       (dump_type t) s (dump_expr e)
  | Comment s -> "// " ^ s
  | Exec e -> dump_expr e
  | Return None -> "Return"
  | Return (Some e) -> "Return " ^ dump_expr e
  | ForEach (typ, ident, expr, body) ->
     Printf.sprintf "ForEach %s %s IN %s %s"
       (dump_type typ) ident
       (dump_expr expr)
       (string_of_list dump_statement body)
  | For (assign, cond, inc, body) ->
     Printf.sprintf "For %s ?: %s %s %s"
       (dump_statement assign)
       (dump_expr cond)
       (dump_expr inc)
       (string_of_list dump_statement body)
  | While (cond, body) ->
     Printf.sprintf "While %s %s"
       (dump_expr cond)
       (string_of_list dump_statement body)
  | Repeat (body, cond) ->
     Printf.sprintf "Repeat %s %s"
       (string_of_list dump_statement body)
       (dump_expr cond)

(* Declarations *)

let dump_method_comp = function
  | Label s -> "Label " ^ s
  | Param (t, s) -> (dump_type t) ^ " " ^ s
  | Identifier s -> "Identifier " ^ s
  | Return_type (is_static, typ) ->
     Printf.sprintf "%sReturn_type %s"
       (if is_static then "Static " else "")
       (dump_type typ)
  | Body l ->
     "Body " ^ string_of_list dump_statement l

let dump_method_arg (label, typ, ident) =
  Printf.sprintf
    "Label %s %s Identifier %s"
    label (dump_type typ) ident

let dump_comment = function
  | Mark s -> "Mark " ^ s
  | LineComment s -> "// " ^ s

let dump_declar = function
  | Method declar ->
     Printf.sprintf
       "Method Return_type %s Identifier %s Arguments: %s Body: %s"
       (dump_type declar.return_type)
       declar.ident
       (string_of_list dump_method_arg declar.params)
       (string_of_list (fun s -> s |> dump_statement |> wrap_in_parens) declar.body)
  | Comment comment -> dump_comment comment

let dump_program (Program declars) =
  string_of_list dump_declar declars
