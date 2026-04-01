
module MUtil = Mvp_lib.Util
module MWrapper = Mvp_lib.Wrapper
module Macro_expand = Mvp_lib.Macro_expand
module SymbolTable = Mvp_lib.Symbol_table
module Semantic = Mvp_lib.Semantic
module Warning = Mvp_lib.Warnings
module Codegen = Mvp_lib.Codegen

let sin_build_do out_opt input = 
  let _output = match out_opt with
    | Some s -> s
    | None -> Filename.remove_extension input
  in
  let ast = MWrapper.parse_input_file input in
  let ast = Macro_expand.expand_macros ast in
  let ast = Util.remove_duplicate_imports ast in
  let symbol_table = SymbolTable.build_symbol_table ast in
  let _ = SymbolTable.check_symbols ast in
  let err_flag = Semantic.check_program ast in
  let has_err = ref false in
  List.iter (fun e -> (
    has_err := true;
    Printf.eprintf "%s\n" e
  )) err_flag;
  if !has_err then (
    exit 1
  );
  let _ = SymbolTable.check_circular_dependencies (input :: symbol_table.files) in
  let warnings = Warning.get_warnings ast in
  List.iter (fun w -> (
    Printf.eprintf "%s\n" w
  )) warnings;
  let cpp_code = Codegen.build_ir input ast in
  let program  = match cpp_code with 
  | [program; _; _] -> program
  | _ -> (
    Printf.eprintf "Error: Codegen failed\n";
    exit 1;
  ) in
  let tmpfile = Filename.temp_file "sinbuild" ".ll" in
  MUtil.write_file tmpfile program;
  let res = Sys.command (Printf.sprintf "g++ %s -o %s" tmpfile _output) in
  if res <> 0 then (
    Printf.eprintf "Error: Compilation failed\n";
  ) else (
    Printf.eprintf "Success: %s\n" _output;
  );
  ()
