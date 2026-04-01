open Tomler
open Parse

let cxx_deal_module name = 
  let nmod = if String.starts_with ~prefix:"std" name then 
    "mvp_std." ^ (String.concat "." ((String.split_on_char '.' name) |> List.tl))
  else name in
  nmod

let cxx_module_cxx name = 
  let nmod = cxx_deal_module name in
  String.concat "::" (String.split_on_char '.' nmod)

let deal_std name = 
  if String.starts_with ~prefix:"std" name then 
    "mvp_std." ^ (String.concat "." ((String.split_on_char '.' name) |> List.tl))
  else name

let write_file filename content =
  let channel = open_out filename in  (* 打开文件，如果存在则覆盖 *)
  output_string channel content;
  close_out channel 

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let get_head lst = 
  match lst with
  | [] -> None
  | h :: _ -> Some h

let string_get_head lst = 
  match get_head lst with
  | Some h -> h
  | None -> ""

let toml_get_codegen_import import = 
  if String.starts_with ~prefix:"c:" import then 
    let import_str = String.sub import 2 (String.length import - 2) in
    "#include <" ^ import_str ^ ">\n"
  else (
    match toml_get_project () with 
    | Some t -> (
      match toml_find_string t "name" with 
      | Some s -> (
        if String.starts_with ~prefix:s import then
          let res = "#include <src/" ^
              (String.concat "/" ((String.split_on_char '/' import) |> List.tl))
              ^ ".h>\n" in
          res
        else
          let pstk = String.split_on_char '/' import in
          let res = "#include <" ^ 
            string_get_head pstk ^ "/src/" ^ 
            (String.concat "/" (List.tl pstk)) ^ ".h>\n" in
          res
      )
      | _ -> ""
    )
    | _ -> ""
  )

let toml_get_codegen_importhere import = 
  if String.starts_with ~prefix:"c:" import then 
    toml_get_codegen_import import
  else (
    let res = toml_get_codegen_import import in
    match toml_from_import import with
    | None -> ""
    | Some raw_module -> (
      let raw_module_name = (parse_input_file raw_module) in
      let raw_module_mod = (Symbol_table.build_symbol_table raw_module_name).module_name in
      let raw_mod_cxx = cxx_module_cxx raw_module_mod in
      res ^ "using namespace " ^ raw_mod_cxx ^ ";\n"
    )
  )

let toml_get_codegen_importas import alias = 
  if String.starts_with ~prefix:"c:" import then 
    toml_get_codegen_import import
  else (
    let res = toml_get_codegen_import import in
    match toml_from_import import with
    | None -> ""
    | Some raw_module -> (
      let raw_module_name = (parse_input_file raw_module) in
      let raw_module_mod = (Symbol_table.build_symbol_table raw_module_name).module_name in
      let raw_mod_cxx = cxx_module_cxx raw_module_mod in
      let alias_mod_cxx = cxx_module_cxx alias in
      res ^ "namespace " ^ alias_mod_cxx ^ " = " ^ raw_mod_cxx ^ ";\n"
    )
  )

let toml_get_mod import = 
  if String.starts_with ~prefix:"c:" import then 
    None
  else (
    match toml_from_import import with
    | None -> None
    | Some raw_module -> (
      let raw_module_name = (parse_input_file raw_module) in
      let raw_module_mod = (Symbol_table.build_symbol_table raw_module_name).module_name in
      Some raw_module_mod
    )
  )

let toml_get_mod_std import = 
  match toml_get_mod import with 
  | Some i -> (
    Some (deal_std i)
  )
  | None -> None

let toml_deal_ias import alias = 
  match toml_get_mod import with 
  | Some i -> (
    let ai = cxx_deal_module alias in 
    let di = cxx_deal_module i in
    Some [di; ai]
  )
  | None -> None

let parse_input_file = Parse.parse_input_file