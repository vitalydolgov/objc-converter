type statement =
  | Dummy
  | Int of int

type method_comp =
  | Label of string
  | Type of string
  | Identifier of string
  | Return_type of string
  | Body of statement list

type declar =
  | Method of { ident : string;
                args : (string * string * string) list;
                return_type : string;
                body : statement list }

type program = Program of declar list

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
        Printf.sprintf "\\([A-Za-z0-9]+\\)\\(%s[A-Za-z0-9]*\\)$" prep
        |> Str.regexp
      in
      try
        let _ = Str.search_forward rex ident 0 in
        let name = Str.matched_group 1 ident in
        let label = Str.matched_group 2 ident |> String.uncapitalize_ascii in
        Some (name, label)
      with
      | Not_found -> None)

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

let dump_statement = function
  | Dummy -> "Dummy"
  | Int i -> "Int " ^ string_of_int i

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
       (string_of_list dump_statement declar.body)

let dump_program (Program declars) =
  string_of_list dump_declar declars
