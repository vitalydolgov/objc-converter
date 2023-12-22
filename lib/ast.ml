type typ =
  | SimpleType of string
  | GenericType of typ * typ
  | Void
  | Any
  | InstanceType
  | Array of typ
  | Optional of typ
  | Protocoled of typ * string
[@@deriving show { with_path = false }]

type ownership =
  | Strong
  | Weak
[@@deriving show { with_path = false }]

type literal =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
[@@deriving show { with_path = false }]

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
  | Negative
[@@deriving show { with_path = false }]

(** Assignment operators. *)
type assignop =
  | Assign
  | IncAssign
  | DecAssign
[@@deriving show { with_path = false }]

(** Basic component of an expression. *)
type atom =
  | Ident of string
  | Literal of literal
  | TypeRef of typ
  | Selector of string
  | Self
  | NoValue
[@@deriving show { with_path = false }]

(** Expression is something that returns a result. *)
and expr =
  | Expr of expr
  | Binary of binop * expr * expr
  | Unary of unary * expr
  | Atom of atom
  | Message of expr * string * (string * arg) list
  | Property of expr * atom
  | Block of typ * (typ * string) list * statement list
  (* return type, parameters with types, body *)
  | TypeCast of typ * expr
  | Element of expr * expr
  | Func of string * expr list
  | ArrayValues of atom list
  | Mutate of assignop * expr * expr
  | Ternary of expr * expr * expr
[@@deriving show { with_path = false }]

(** Statement is something that returns no result. *)
and statement =
  | If of expr * statement list
  | Else of [`NoCond of statement list | `Cond of statement]
  | NewVar of ownership * typ * string * expr
  | Comment of string
  | Exec of expr
  | Return of expr option
  | ForEach of typ * string * expr * statement list
  | For of statement * expr * expr * statement list
  | While of expr * statement list
  | Repeat of statement list * expr
[@@deriving show { with_path = false }]

and arg =
  | NormalArg of expr
  | VarArg of expr list
[@@deriving show { with_path = false }]

type comment =
  | Mark of string
  | LineComment of string
[@@deriving show { with_path = false }]

(** Declaration that compose a program. *)
type declar =
  | Method of { is_static : bool;
                ident : string;
                params : (string * typ * string) list;
                         (* label, type, name *)
                return_type : typ;
                body : statement list }
  | Comment of comment
[@@deriving show { with_path = false }]

type program =
  | Program of declar list
  | Statement of statement

(* Helper types *)

type method_comp =
  | Label of string
  | Param of typ * string
  | Identifier of string
  | Return_type of bool * typ
  (* static, type *)
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
  | t -> SimpleType t

let make_generic_type g t =
  match g with
  | "NSArray" | "NSMutableArray" -> Array (make_type t)
  | _ -> GenericType (make_type g, make_type t)

let make_protocol_type t p =
  Protocoled (make_type t, p)

(* Debug *)

let string_of_list to_string lis =
  "[" ^ (String.concat "; " (List.map to_string lis)) ^ "]"

let dump_program = function
  | Program declars -> string_of_list show_declar declars
  | Statement statement -> show_statement statement
