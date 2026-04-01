open Printf
open Util 

open Mvp_lib.Env
open Mvp_lib.Wrapper

module Semantic = Mvp_lib.Semantic
module Macro_expand = Mvp_lib.Macro_expand
module SymbolTable = Mvp_lib.Symbol_table
module Warning = Mvp_lib.Warnings
module Codegen = Mvp_lib.Codegen

let compile_cpp ~verbose ~_input_file ~output_file ~project_type ~release =
  let build_dir = get_build_dir () in
  let cache_dir = get_cache_dir () in
  let std_include = get_std_include_dir () in
  
  let base_name = Filename.remove_extension _input_file in
  let unique_base = base_name in
  
  let cpp_file = unique_base ^ ".cpp" in
  let header_file = unique_base ^ ".h" in
  let obj_file = unique_base ^ ".o" in
  let exe_file = output_file in
  
  ensure_dir_for_file cpp_file;
  ensure_dir_for_file header_file;
  ensure_dir_for_file exe_file;
  
  let incf = get_include_flag () in
  let cmd =
    sprintf "g++ %s %s -o %s -I%s -I%s -I%s -std=c++20 %s %s -c %s"
      (if release then "-O2" else "-g")
      cpp_file obj_file cache_dir std_include build_dir 
      (if verbose then "" else "2>/dev/null") 
      (if String.compare project_type "lib" == 0 then "-fPIC" else "") incf
  in
  
  if Sys.command cmd <> 0 then (
    if not verbose then (
      failwith "compilation failed"
    );
  );
  
  if not (get_keep_cpp ()) then (
    ignore (Sys.command (sprintf "rm -f %s" cpp_file));
  );
  
  if verbose then eprintf "\nCompiled successfully: %s\n%!" obj_file;
  obj_file

let link_file ~obj_files ~output_file ~project_type =
  let build_dir = get_build_dir () in
  let exe_file = build_dir ^ "/" ^ (
    if String.compare project_type "lib" == 0 then 
      "lib" ^ output_file
    else 
      output_file
  ) in
  let lnkf = get_link_flag () in
  let cmd =
    sprintf "g++ -O2 %s -o %s -std=c++20 %s %s"
      (String.concat " " obj_files) 
      (if String.compare project_type "lib" == 0 then exe_file ^ ".so" else exe_file) lnkf 
      (if String.compare project_type "lib" == 0 then "-fPIC -shared" else "")
  in
  if Sys.command cmd <> 0 then (
    eprintf "\x1b[31mError \x1b[0mlinking failed%!";
    exit 1
  ) else (
    eprintf "\n\x1b[34mLinking \x1b[0mfiles%!"
  );
  exe_file

let compile_program_obj ~input_file ~output_file ~_release ~with_warn =  
  let all_err = ref [] in
  let ast = parse_input_file input_file in
  let ast = Macro_expand.expand_macros ast in
  let ast = remove_duplicate_imports ast in

  let symbol_table, errs = SymbolTable.build_symbol_table_werr ast in
  all_err := errs @ !all_err;
  let errs = Semantic.check_program ast in
  let errs = List.map (fun e -> (
    "Error:" ^ e
  )) errs in
  all_err := errs @ !all_err;
  let errs = SymbolTable.check_circular_dependencies (input_file :: symbol_table.files) in
  all_err := errs @ !all_err;
  let warnings = Warning.get_warnings ast in

  eprintf "%s\x1b[31m%s\x1b[0m%!" 
    (if !all_err <> [] then "\n" else "")
    (String.concat "\n" !all_err);
  if with_warn then (
    eprintf "%s\x1b[33m%s\x1b[0m%!" 
      (if warnings <> [] then "\n" else "")
      (String.concat "\n" warnings);
  );

  let cpp_code = Codegen.build_ir input_file ast in

  let cache_dir = get_cache_dir_rel _release in
  let base_name = Filename.remove_extension output_file in
  let unique_base = base_name in
  let cpp_file = cache_dir ^ "/" ^ unique_base ^ ".cpp" in
  let cpp_test_file = cache_dir ^ "/" ^ unique_base ^ ".test.cpp" in
  let header_file = cache_dir ^ "/" ^ unique_base ^ ".h" in
  let header_real = unique_base ^ ".h" in

  ensure_dir_for_file cpp_file;
  ensure_dir_for_file header_file;

  let cpp_code_, header_code, test_code = match cpp_code with
    | [cpp; header; test] -> (cpp, header, 
        (if String.length test > 2 then (
          "#include <" ^ header_real ^ ">\n" ^ test
        ) else (""))
      )
    | _ -> failwith "Unexpected codegen output format"
  in

  let oc = open_out cpp_file in
  output_string oc cpp_code_;
  close_out oc;

  if String.length test_code > 0 then (
    let oc = open_out cpp_test_file in
    output_string oc test_code;
    close_out oc;
  );
  
  if header_code <> "" then (
    let oc_header = open_out header_file in
    output_string oc_header header_code;
    close_out oc_header;
  );
  cpp_file, !all_err <> []
