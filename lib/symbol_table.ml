[@@@ocaml.warning "-11"]
[@@@ocaml.warning "-27"]
open Ast

module StringSet = Set.Make(String)


(* 辅助函数：查找元素在列表中的索引 *)
let find_index elem list =
  let rec find_index_helper elem list index =
    match list with
    | [] -> None
    | h :: t -> if h = elem then Some index else find_index_helper elem t (index + 1)
  in
  find_index_helper elem list 0

(* 符号表类型定义 *)
type symbol_table = {
  module_name: string;                              (* 模块名 *)
  functions: (string * param list * typ option * function_safety) list; (* 函数名 -> 参数列表和返回类型和安全属性 *)
  structs: (string * (string * typ) list) list;      (* 结构体名 -> 字段列表 *)
  exported_functions: (string * param list * typ option * function_safety) list;                  (* 导出的函数名列表 *)
  files: string list
}

(* 函数安全属性 *)
and function_safety = 
  | Safe
  | Unsafe 
  | Trusted

(* 空符号表 *)
let empty_symbol_table = {
  module_name = "";
  functions = [];
  structs = [];
  exported_functions = [];
  files = [];
}

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let get_head list = 
  match list with
  | h :: _ -> h
  | [] -> ""

(* 辅助函数：丢弃列表前n个元素 *)
let rec drop n list =
  if n <= 0 then list
  else
    match list with
    | [] -> []
    | _ :: t -> drop (n - 1) t

(* 检查函数是否重复定义 *)
let check_function_duplicate name params _return_typ symbol_table =
  let existing_funcs = List.filter (fun (n, _, _, _) -> n = name) symbol_table.functions in
  if List.length existing_funcs > 0 then
    let existing_params = List.map (fun (_, p, _, _) -> p) existing_funcs in
    (* 检查参数列表是否相同 - 不允许函数重载 *)
    if List.exists (fun p -> p = params) existing_params then
      failwith ("Function '" ^ name ^ "' is already defined")
    else
      (* 即使参数不同也不允许重载 *)
      failwith ("Function '" ^ name ^ "' is already defined (function overloading is not allowed)")
  else
      ()

(* 检查结构体是否重复定义 *)
let check_struct_duplicate name symbol_table =
  let existing_structs = List.filter (fun (n, _) -> n = name) symbol_table.structs in
  if List.length existing_structs > 0 then
    failwith ("Struct '" ^ name ^ "' is already defined")
  else
    ()

(* 添加函数到符号表 *)
let add_function name params return_typ safety symbol_table =
  check_function_duplicate name params return_typ symbol_table;
  { symbol_table with 
    functions = (name, params, return_typ, safety) :: symbol_table.functions 
  }

(* 添加结构体到符号表 *)
let add_struct name fields symbol_table =
  check_struct_duplicate name symbol_table;
  { symbol_table with 
    structs = (name, fields) :: symbol_table.structs 
  }

(* 处理单个定义，构建符号表 *)
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
      (* 测试函数不计入符号表 *)
      symbol_table
  | DCFuncUnsafe (_, name, params, return_typ, _) ->
      add_function name params return_typ Unsafe symbol_table
  | DModule (_, name) -> (
    { symbol_table with module_name = name }
  )
  | SImport (_, import) -> (
    if String.starts_with ~prefix:"c:" import then 
      symbol_table
    else
    let toml = read_file "miva.toml" in
    let file = match Toml.Parser.from_string toml with 
    | `Ok table -> (
      try
        match Toml.Types.Table.find (Toml.Min.key "project") table with 
        | Toml.Types.TTable t -> (
          match Toml.Types.Table.find (Toml.Min.key "name") t with 
          | Toml.Types.TString s -> (
            if String.starts_with ~prefix:s import then 
              let res = String.concat "/" ((String.split_on_char '/' import) |> List.tl) in
              let ret = "src/" ^ res ^ ".miva" in
              ret
            else 
              let stdpath = try Sys.getenv "MIVA_STD" with _ -> "." in
              let pstack = String.split_on_char '/' import in
              stdpath ^ "/" ^ get_head pstack ^ "/src/" ^ (String.concat "/" (List.tl pstack)) ^ ".miva"
          )
          | _ -> ""
        )
        | _ -> ""
      with 
        | _ -> ""
    );
    | _ -> "" in
    if file <> "" then
      { symbol_table with files = file :: symbol_table.files }
    else 
      symbol_table
  )
  | SExport (_, name) -> (
    (* 检查是否是函数，如果是则添加到导出函数列表 *)
    let is_function = List.exists (fun (fname, _, _, _) -> fname = name) symbol_table.functions in
    if is_function then
      let func = List.find (fun (fname, _, _, _) -> fname = name) symbol_table.functions in
      { symbol_table with exported_functions = func :: symbol_table.exported_functions }
    else
      symbol_table
  )
  | _ -> symbol_table

(* 构建整个程序的符号表 *)
let build_symbol_table defs =
  List.fold_left (fun sym_table def -> process_definition def sym_table) empty_symbol_table defs

(* 检查循环依赖 *)
let check_circular_dependencies file_paths =
  let rec visit file visited stack =
    if List.mem file stack then
      (match find_index file stack with
      | Some cycle_start ->
          let _ = drop cycle_start stack @ [file] in
          failwith ("\nCircular dependency detected")
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
        let symbol_table = build_symbol_table ast in
        List.iter (fun dep_file ->
          visit dep_file new_visited new_stack
        ) symbol_table.files
      with
      | Parsing.Parse_error | Lexer.SyntaxError _ ->
          (* 如果解析失败，跳过这个文件的依赖检查 *)
          ()
  in
  let visited = ref [] in
  let stack = ref [] in
  List.iter (fun file ->
    if not (List.mem file !visited) then
      visit file !visited !stack
  ) file_paths

(* 获取函数的安全属性 *)
let get_function_safety name symbol_table =
  let funcs = List.filter (fun (n, _, _, _) -> n = name) symbol_table.functions in
  match funcs with
  | [(_, _, _, safety)] -> Some safety
  | [] -> None
  | _ -> failwith ("Multiple definitions of function: " ^ name)

(* 构建完整的依赖图 *)
let build_dependency_graph filename symbol_table =
  let graph = Dependency_graph.create () in
  
  let parse_input_file filename =
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    Lexing.set_filename lexbuf filename;
    try
      let ast = Parser.program Lexer.token lexbuf in
      close_in ic;
      ast
    with
    | Parsing.Parse_error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
        Printf.eprintf "Syntax error at %s:%d:%d\n%!" filename line col;
        exit 1
    | Lexer.SyntaxError msg ->
        Printf.eprintf "Lexer error: %s\n%!" msg;
        exit 1
    | _ ->
        Printf.eprintf "Parsing error\n%!";
        exit 1 in
  
  let rec add_deps file visited =
    let visited = StringSet.add file visited in
    try
      let ast = parse_input_file file in
      let dep_table = build_symbol_table ast in
      (* 添加依赖关系 *)
      List.fold_left (fun acc dep_file ->
        Dependency_graph.add_dependency graph file dep_file;
        add_deps dep_file acc
      ) visited dep_table.files
    with
    | Parsing.Parse_error | Lexer.SyntaxError _ -> (
        Printf.eprintf "error found\n%!" ;
        (* 如果解析失败，只返回当前文件 *)
        visited
    )
  in
  
  (* 从根文件开始收集所有依赖并添加到图中 *)
  let _ = add_deps filename StringSet.empty in
  graph

(* 检查程序中的符号重复定义 *)
let check_symbols defs =
  let _ = build_symbol_table defs in
  () (* 如果没有异常抛出，说明检查通过 *)