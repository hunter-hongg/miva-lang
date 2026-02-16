[@@@ocaml.warning "-21"]
open Printf
open Cmdliner

module Ast = Mvp_lib.Ast
module Lexer = Mvp_lib.Lexer
module Parser = Mvp_lib.Parser
module Semantic = Mvp_lib.Semantic
module Codegen = Mvp_lib.Codegen

(* ---------- 全局选项 ---------- *)
let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

(* ---------- 公共解析函数 ---------- *)
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
      eprintf "Syntax error at %s:%d:%d\n%!" filename line col;
      exit 1
  | Lexer.SyntaxError msg ->
      eprintf "Lexer error: %s\n%!" msg;
      exit 1

let check_semantics ast =
  try Semantic.check_program ast
  with Failure msg ->
    eprintf "Semantic error: %s\n%!" msg;
    exit 1

let generate_cpp_code inp ast =
  try Codegen.build_ir inp ast
  with Failure msg ->
    eprintf "Codegen error: %s\n%!" msg;
    exit 1

(* ---------- 构建目录管理 ---------- *)
let get_build_dir () =
  try Sys.getenv "MVP_BUILD" with Not_found -> "build"

let get_std_include_dir () =
  try Sys.getenv "MVP_STD" with Not_found -> "util"

let ensure_dir_for_file file_path =
  let dir = Filename.dirname file_path in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (sprintf "mkdir -p %s" dir))

let compile_cpp ~verbose ~_input_file ~output_file cpp_code =
  let build_dir = get_build_dir () in
  let std_include = get_std_include_dir () in
  
  (* 生成唯一的构建文件名，避免冲突 *)
  let timestamp = string_of_float (Unix.gettimeofday ()) in
  let hash_hex = Digest.string timestamp |> Digest.to_hex in
  let unique_suffix = String.sub hash_hex 0 8 in
  let base_name = Filename.remove_extension output_file in
  let unique_base = base_name ^ "_" ^ unique_suffix in
  
  let cpp_file = build_dir ^ "/" ^ unique_base ^ ".cpp" in
  let exe_file = build_dir ^ "/" ^ unique_base in
  
  (* 确保文件路径的目录存在 *)
  ensure_dir_for_file cpp_file;
  ensure_dir_for_file exe_file;
  
  (* 写入C++源代码 *)
  let oc = open_out cpp_file in
  output_string oc cpp_code;
  close_out oc;

  (* 编译命令 *)
  let cmd =
    sprintf "g++ -O2 %s -o %s -I%s -std=c++20 %s"
      cpp_file exe_file std_include (if verbose then "" else "2>/dev/null")
  in
  
  if Sys.command cmd <> 0 then (
    if not verbose then (
      (* 重试并显示详细输出 *)
      let cmd_verbose =
        sprintf "g++ -O2 %s -o %s -I%s -std=c++20" cpp_file exe_file std_include
      in
      eprintf "Compilation failed. Running: %s\n%!" cmd_verbose;
      ignore (Sys.command cmd_verbose)
    );
    eprintf "C++ code saved to %s\n%!" cpp_file;
    exit 1
  );
  
  (* 清理中间文件，除非设置了保留标志 *)
  if not (try Sys.getenv "MVP_KEEP_CPP" <> "" with Not_found -> false) then
    ignore (Sys.command (sprintf "rm -f %s" cpp_file));
  
  if verbose then eprintf "Compiled successfully: %s\n%!" exe_file;
  exe_file

(* ---------- 公共编译流程函数 ---------- *)
let compile_program ~verbose ~input_file ~output_file =
  if verbose then eprintf "Parsing %s...\n%!" input_file;
  let ast = parse_input_file input_file in
  if verbose then eprintf "Checking semantics...\n%!";
  check_semantics ast;
  if verbose then eprintf "Generating C++ code...\n%!";
  let cpp_code = generate_cpp_code input_file ast in
  if verbose then eprintf "Compiling with g++...\n%!";
  let exe_path = compile_cpp ~verbose ~_input_file:input_file ~output_file:output_file cpp_code in
  exe_path

(* ---------- 子命令: build ---------- *)
let build_input_file =
  let doc = "MVP source file to compile" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let build_output_file =
  let doc = "Output executable name (default: <input> without extension)" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let build_cmd =
  let doc = "Compile an MVP source file to an executable." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(
    const (fun verbose input out_opt () ->
        let output = match out_opt with
          | Some s -> s
          | None -> Filename.remove_extension input
        in
        let exe_path = compile_program ~verbose ~input_file:input ~output_file:output in
        printf "Compiled successfully: %s\n%!" exe_path
      )
    $ verbose
    $ build_input_file
    $ build_output_file
    $ const ()
  )

(* ---------- 子命令: run ---------- *)
let run_input_file =
  let doc = "MVP source file to run" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let run_cmd =
  let doc = "Compile and run an MVP program." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(
    const (fun verbose input () ->
        let output = Filename.basename (Filename.remove_extension input) in
        let exe_path = compile_program ~verbose ~input_file:input ~output_file:output in
        if verbose then eprintf "Running %s...\n%!" exe_path;
        let run_status = Sys.command exe_path in
        (* 清理可执行文件，除非设置了保留标志 *)
        if not (try Sys.getenv "MVP_KEEP_EXE" <> "" with Not_found -> false) then
          ignore (Sys.command (sprintf "rm -f %s" exe_path));
        exit run_status
      )
    $ verbose
    $ run_input_file
    $ const ()
  )

(* ---------- 主命令 ---------- *)
let default_cmd =
  let doc = "The MVP programming language compiler" in
  let man = [
    `S "DESCRIPTION";
    `P "MVP is a systems programming language focused on explicitness, safety, and predictability.";
  ] in
  let info = Cmd.info "mvp" ~version:"0.1.0" ~doc ~man in
  Cmd.group info [build_cmd; run_cmd]

let () =
  exit @@ Cmd.eval default_cmd