[@@@ocaml.warning "-11"]
[@@@ocaml.warning "-27"]
open Ast
open Util
open Tomler
open Parse

module StringSet = Set.Make(String)

let find_index elem list =
  let rec find_index_helper elem list index =
    match list with
    | [] -> None
    | h :: t -> if h = elem then Some index else find_index_helper elem t (index + 1)
  in
  find_index_helper elem list 0

type symbol_table = {
  module_name: string; 
  functions: (string * param list * typ option * function_safety) list; 
  structs: (string * (string * typ) list) list; 
  exported_functions: (string * param list * typ option * function_safety) list;  
  files: string list;
  imports: string list;
}

and function_safety = 
  | Safe
  | Unsafe 
  | Trusted

let empty_symbol_table = {
  module_name = "";
  functions = [];
  structs = [];
  exported_functions = [];
  files = [];
  imports = [];
}

let get_head = Util.string_get_head

let check_function_duplicate name params _return_typ symbol_table =
  let errs = ref [] in
  let existing_funcs = List.filter (fun (n, _, _, _) -> n = name) symbol_table.functions in
  if List.length existing_funcs > 0 then
    errs := ("Error:function '" ^ name ^ "' is already defined") :: !errs
  else
    ();
  !errs

let check_struct_duplicate name symbol_table =
  let errs = ref [] in
  let existing_structs = List.filter (fun (n, _) -> n = name) symbol_table.structs in
  if List.length existing_structs > 0 then
    errs := ("Error:struct '" ^ name ^ "' is already defined") :: !errs
  else
    ();
  !errs

let add_function name params return_typ safety symbol_table =
  let errs = check_function_duplicate name params return_typ symbol_table in
  { symbol_table with 
    functions = (name, params, return_typ, safety) :: symbol_table.functions 
  }, errs

let add_struct name fields symbol_table =
  let errs = check_struct_duplicate name symbol_table in
  { symbol_table with 
    structs = (name, fields) :: symbol_table.structs 
  }, errs

let process_definition def symbol_table =
  match def with
  | DStruct (_, name, fields) ->
      add_struct name fields symbol_table
  | DFunc (_, name, params, return_typ, _) ->
      add_function name params return_typ Safe symbol_table
  | DFuncUnsafe (_, name, params, return_typ, _) ->
      add_function name params return_typ Unsafe symbol_table
  | DFuncTrusted (_, name, params, return_typ, _) ->
      add_function name params return_typ Trusted symbol_table
  | DTest (_, _, _) ->
      symbol_table, []
  | DCFuncUnsafe (_, name, params, return_typ, _) ->
      add_function name params return_typ Unsafe symbol_table
  | DModule (_, name) -> (
    { symbol_table with module_name = name }, []
  )
  | SImport (_, import) -> (
    match toml_from_import import with
    | None -> symbol_table, []
    | Some file -> (
      if file <> "" then
        { symbol_table with files = file :: symbol_table.files; imports = import :: symbol_table.imports }, []
      else 
        { symbol_table with imports = import :: symbol_table.imports }, []
    )
  );
  | SImportAs (_, import, _) -> (
    match toml_from_import import with
    | None -> symbol_table, []
    | Some file -> (
      if file <> "" then
        { symbol_table with files = file :: symbol_table.files }, []
      else 
        symbol_table, []
    )
  )
  | SImportHere (_, import) -> (
    match toml_from_import import with
    | None -> symbol_table, []
    | Some file -> (
      if file <> "" then
        { symbol_table with files = file :: symbol_table.files }, []
      else 
        symbol_table, []
    )
  )
  | SExport (_, name) -> (
    let is_function = List.exists (fun (fname, _, _, _) -> fname = name) symbol_table.functions in
    if is_function then
      let func = List.find (fun (fname, _, _, _) -> fname = name) symbol_table.functions in
      { symbol_table with exported_functions = func :: symbol_table.exported_functions }, []
    else
      symbol_table, []
  )
  | _ -> symbol_table, []

let build_symbol_table_werr defs =
  let errrs = ref [] in
  let r = List.fold_left (fun sym_table def -> (
    let s, errs = process_definition def sym_table in
    errrs := errs @ !errrs;
    s
  )) empty_symbol_table defs in
  r, !errrs

let build_symbol_table defs = 
  let s, _ = build_symbol_table_werr defs in
  s

let check_circular_dependencies file_paths =
  let errs = ref [] in
  let rec visit file visited stack =
    if List.mem file stack then
      (match find_index file stack with
      | Some cycle_start ->
          errs := "Error:circular dependency detected" :: !errs
      | None -> ()
      )
    else if List.mem file visited then
      ()
    else
      let new_visited = file :: visited in
      let new_stack = file :: stack in
      try
        let file_content = read_file file in
        let lexbuf = Lexing.from_string file_content in
        let ast = Parser.program Lexer.token lexbuf in
        let symbol_table, _ = build_symbol_table_werr ast in
        List.iter (fun dep_file ->
          visit dep_file new_visited new_stack
        ) symbol_table.files
      with
      | Parsing.Parse_error | Lexer.SyntaxError _ ->
        ()
  in
  let visited = ref [] in
  let stack = ref [] in
  List.iter (fun file ->
    if not (List.mem file !visited) then
      visit file !visited !stack
  ) file_paths;
  !errs

let get_function_safety name symbol_table =
  let funcs = List.filter (fun (n, _, _, _) -> n = name) symbol_table.functions in
  match funcs with
  | [(_, _, _, safety)] -> Some safety
  | [] -> None
  | _ -> failwith ("multiple definitions of function: " ^ name)

let build_dependency_graph filename symbol_table =
  let graph = Dependency_graph.create () in
  
  let rec add_deps file visited =
    let visited = StringSet.add file visited in
    try
      let ast = parse_input_file file in
      let dep_table, _ = build_symbol_table_werr ast in
      List.fold_left (fun acc dep_file ->
        Dependency_graph.add_dependency graph file dep_file;
        add_deps dep_file acc
      ) visited dep_table.files
    with
    | Parsing.Parse_error | Lexer.SyntaxError _ -> (
        visited
    )
  in
  
  let _ = add_deps filename StringSet.empty in
  graph

let check_symbols defs =
  let _, errs = build_symbol_table_werr defs in
  errs
