open Mvp_frontend_lib

let () =
  match Array.length Sys.argv with
  | 2 ->
      let input_file = Sys.argv.(1) in
      let ast = Parse.parse_input_file input_file in
      print_string (Ast_to_json.ast_to_json input_file ast)
  | 4 when Sys.argv.(2) = "-o" ->
      let input_file = Sys.argv.(1) in
      let output_file = Sys.argv.(3) in
      let ast = Parse.parse_input_file input_file in
      Ast_to_json.save_ast input_file ast output_file
  | _ ->
      Printf.eprintf "Usage: miva-frontend <input.miva> [-o output.ast.json]\n";
      exit 1
