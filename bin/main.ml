[@@@ocaml.warning "-21"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-26"]
open Printf
open Cmdliner

module Ast = Mvp_lib.Ast
module Lexer = Mvp_lib.Lexer
module Parser = Mvp_lib.Parser
module Semantic = Mvp_lib.Semantic
module SymbolTable = Mvp_lib.Symbol_table
module Codegen = Mvp_lib.Codegen
module Global = Mvp_lib.Global
let list_files dir =
  let dh = Unix.opendir dir in
  let rec read_files acc =
    try
      let entry = Unix.readdir dh in
      if entry = "." || entry = ".." then
        read_files acc
      else
        read_files (entry :: acc)
    with End_of_file ->
      Unix.closedir dh;
      List.rev acc
  in
  read_files []
let write_file filename content =
  let channel = open_out filename in  (* 打开文件，如果存在则覆盖 *)
  output_string channel content;
  close_out channel 

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content
let remove_prefix (a : string) (b : string) : string =
  let len_a = String.length a in
  let len_b = String.length b in
  (* 从索引 len_a 开始，截取 len_b - len_a 个字符 *)
  String.sub b len_a (len_b - len_a)

let clean_line () =
  Printf.eprintf "%s" ("\r" ^ (String.make 100 ' '))

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
  | _ ->
      eprintf "Parsing error\n%!";
      exit 1

let check_semantics ast =
  try Semantic.check_program ast
  with Failure msg ->
    eprintf "\nSemantic error: %s\n%!" msg;
    exit 1

let check_symbol_table ast =
  try SymbolTable.check_symbols ast
  with Failure msg ->
    eprintf "Symbol table error: %s\n%!" msg;
    exit 1

let check_circular_dependencies file_paths =
  try SymbolTable.check_circular_dependencies file_paths
  with Failure msg ->
    eprintf "\nCircular dependency error: %s\n%!" msg;
    exit 1

let generate_cpp_code inp ast =
  try Codegen.build_ir inp ast
  with Failure msg ->
    eprintf "Codegen error: %s\n%!" msg;
    exit 1


(* ---------- 构建目录管理 ---------- *)
let get_build_dir () =
  try Sys.getenv "MIVA_BUILD" with Not_found -> "build"

let get_cache_dir () =
  try Sys.getenv "MIVA_BUILD_CACHE" with Not_found -> "build/cache"

let get_std_include_dir () =
  try Sys.getenv "MIVA_STD" with Not_found -> "util"

let get_include_flag () = 
  try Sys.getenv "MIVA_INC_FLAGS" with Not_found -> ""

let get_link_flag () = 
  try Sys.getenv "MIVA_LINK_FLAGS" with Not_found -> ""

let ensure_dir_for_file file_path =
  let dir = Filename.dirname file_path in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (sprintf "mkdir -p %s" dir))

let compile_cpp ~verbose ~_input_file ~output_file =
  let build_dir = get_build_dir () in
  let cache_dir = get_cache_dir () in
  let std_include = get_std_include_dir () in
  
  let base_name = Filename.remove_extension _input_file in
  let unique_base = base_name in
  
  let cpp_file = unique_base ^ ".cpp" in
  let header_file = unique_base ^ ".h" in
  let obj_file = unique_base ^ ".o" in
  let exe_file = output_file in
  
  (* 确保文件路径的目录存在 *)
  ensure_dir_for_file cpp_file;
  ensure_dir_for_file header_file;
  ensure_dir_for_file exe_file;
  
  let incf = get_include_flag () in
  (* 编译命令 *)
  let cmd =
    sprintf "g++ -O2 %s -o %s -I%s -I%s -I%s -std=c++20 %s -c %s"
      cpp_file obj_file cache_dir std_include build_dir (if verbose then "" else "2>/dev/null") incf
  in
  
  if Sys.command cmd <> 0 then (
    if not verbose then (
      (* 重试并显示详细输出 *)
      let cmd_verbose =
        cmd
      in
      eprintf "\nCompilation failed. Running: %s\n%!" cmd_verbose;
      ignore (Sys.command cmd_verbose)
    );
    eprintf "C++ code saved to %s\n%!" cpp_file;
    exit 1
  );
  
  (* 清理中间文件，除非设置了保留标志 *)
  if not (try Sys.getenv "MIVA_KEEP_CPP" <> "" with Not_found -> false) then (
    ignore (Sys.command (sprintf "rm -f %s" cpp_file));
  );
  
  if verbose then eprintf "\nCompiled successfully: %s\n%!" obj_file;
  obj_file

let link_file ~verbose ~obj_files ~output_file =
  let build_dir = get_build_dir () in
  let exe_file = build_dir ^ "/" ^ output_file in
  let lnkf = get_link_flag () in
  let cmd =
    sprintf "g++ -O2 %s -o %s -std=c++20 %s"
      (String.concat " " obj_files) exe_file lnkf
  in
  if Sys.command cmd <> 0 then (
    if not verbose then (
      (* 重试并显示详细输出 *)
      let cmd_verbose =
        sprintf "g++ -O2 %s -o %s -std=c++20"
          (String.concat " " obj_files) exe_file
      in
      eprintf "Linking failed. Running: %s\n%!" cmd_verbose;
      ignore (Sys.command cmd_verbose)
    );
    eprintf "Linking failed: %s\n%!" exe_file;
    exit 1
  );
  if verbose then eprintf "Linked successfully: %s\n%!" exe_file;
  exe_file

(* ---------- 公共编译流程函数 ---------- *)
let compile_program_obj ~verbose ~input_file ~output_file =
  if verbose then eprintf "Parsing %s...\n%!" input_file;
  let ast = parse_input_file input_file in
  if verbose then eprintf "Checking semantics...\n%!";
  check_semantics ast;
  if verbose then eprintf "Checking symbol table...\n%!";
  check_symbol_table ast;
  if verbose then eprintf "Checking circular dependencies...\n%!";
  let symbol_table = SymbolTable.build_symbol_table ast in
  check_circular_dependencies (input_file :: symbol_table.files);
  if verbose then eprintf "Generating C++ code...\n%!";
  let cpp_code = generate_cpp_code input_file ast in
  let cache_dir = get_cache_dir () in
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
  
  (* 写入C++源代码 *)
  let oc = open_out cpp_file in
  output_string oc cpp_code_;
  close_out oc;

  if String.length test_code > 0 then (
    let oc = open_out cpp_test_file in
    output_string oc test_code;
    close_out oc;
  ) else (
    ()
  );
  
  (* 写入头文件（如果头文件内容不为空） *)
  if header_code <> "" then (
    let oc_header = open_out header_file in
    output_string oc_header header_code;
    close_out oc_header;
    if verbose then eprintf "Generated header file: %s\n%!" header_file;
  );
  cpp_file
let compile_program ~verbose ~input_file ~output_file =
  let ast = parse_input_file input_file in
  let symtab = SymbolTable.build_symbol_table ast in
  let queue = input_file :: symtab.files in
  let hist = [] in
  (* 实现代码：编译文件*)
  let rec process_queue queue hist obj_paths = 
    match queue with
    | [] -> obj_paths
    | s :: rest ->
        let snw = String.concat "/" (List.filter (fun s -> s <> "") (String.split_on_char '/' s)) in
        clean_line ();
        eprintf "\rCompiling %s to cpp file ...%!" snw;
        let symt = SymbolTable.build_symbol_table (parse_input_file s) in
        let stdp = get_std_include_dir () in
        let objq = if String.starts_with ~prefix:stdp s then (
          compile_program_obj ~verbose ~input_file:s ~output_file:(
            Filename.remove_extension (remove_prefix stdp s)
          )
        ) else (
          compile_program_obj ~verbose ~input_file:s ~output_file:(Filename.remove_extension s)
        ) in
        (* s出队列queue进入历史记录hist，如果queue和hist都没有objp，objp入queue *)
        let new_hist = s :: hist in
        let rest2 = rest in
        let rec process_files files queue hist =
          match files with
          | [] -> queue, hist
          | objp :: rest ->
              let new_queue = if not (List.mem objp queue) && not (List.mem objp hist) then (
                 queue @ [objp] 
              )else (
                queue
              ) in
              process_files rest new_queue hist
        in
        let new_queue, new_hist= process_files symt.files rest2 new_hist in
        process_queue new_queue new_hist (objq :: obj_paths)
  in
  let obj_paths = process_queue queue hist [] in
  let obj_paths_real = List.map (fun s -> (
    let snw = String.concat "/" (List.filter (fun s -> s <> "") (String.split_on_char '/' s)) in
    clean_line ();
    Printf.eprintf "\r%!Compiling %s to bin...%!" snw;
    compile_cpp ~verbose ~_input_file:s ~output_file:(Filename.basename s)
  )) obj_paths in
  let exe_path = link_file ~verbose:verbose ~obj_files:obj_paths_real ~output_file:output_file in
  exe_path

(* ---------- 子命令: init ---------- *)
let init_name = 
  let doc = "Name of the project to initialize" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let init_type = 
  let doc = "Type of project to initialize (e.g., 'bin', 'lib')" in
  Arg.(required & opt (some string) None & info ["t"; "type"] ~doc)

let init_cmd = 
  let doc = "Initialize a new Miva project" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(
    const (fun _verbose init_name init_type () ->
        if Sys.file_exists "miva.toml" then (
          eprintf "Error: Project already initialized in this directory.\n%!";
          exit 1;
        ) else (
          match init_type with
          | "bin" | "lib" -> (
            write_file "miva.toml" (
              sprintf "[project]\nname = \"%s\"\ntype = \"%s\"\n" init_name init_type
            );
            Sys.mkdir "src" 0o755;
            if init_type == "bin" then 
              write_file "src/main.miva" (
                "// main.miva \n" ^
                "// Generated by Miva init\n" ^
                "module \"main\"\n" ^
                "main = () => {\n" ^
                "  println(\"Hello, World\")\n" ^ 
                "}\n"
              );
            printf "Initialized %s project in %s\n%!" init_type init_name
          )
          | _ -> (
            eprintf "Error: Unknown project type: %s\n%!" init_type;
            exit 1;
          )
        )
      )
    $ verbose
    $ init_name
    $ init_type
    $ const ()
  )

(* ---------- 子命令: build/run ---------- *)
let build_project ~verbose = 
  if not (Sys.file_exists "miva.toml") then (
    eprintf "Error: Project not initialized in this directory.\n%!";
    exit 1;
  ) else (
    let toml = read_file "miva.toml" in
    match Toml.Parser.from_string toml with 
    | `Ok table -> (
      let project_table = 
      try 
        match Toml.Types.Table.find (Toml.Min.key "project") table with 
        | Toml.Types.TTable t -> (
          let res = match Toml.Types.Table.find (Toml.Min.key "name") t with 
          | Toml.Types.TString s -> s
          | _ -> (
            eprintf "Error: 'name' in 'project' table in miva.toml is not a string.\n%!";
            exit 1;
          ) in 
          match Toml.Types.Table.find (Toml.Min.key "type") t with 
          | Toml.Types.TString s -> (
            if String.compare s "lib" == 0 then (
              eprintf "Error: Build a library is not supported in v0.0.2.\n";
              eprintf "This feature will come in v0.0.3.\n%!";
              exit 1;
            ) else (
              res
            )
          )
          | _ -> (
            eprintf "Error: 'type' in 'project' table in miva.toml is not a string.\n%!";
            exit 1;
          )
        ) 
        | _ -> (
          eprintf "Error: 'project' table in miva.toml is not a table.\n%!";
          exit 1;
        )
      with  
      | _ -> (
        eprintf "Error: Failed to find 'project' table or 'name' in 'project' table in miva.toml.\n%!";
        exit 1;
      ) in
      let input_file = "src/main.miva" in
      let output_file = project_table in
      let exe_path = compile_program ~verbose ~input_file ~output_file in
      printf "\n%s%!" (String.make 30 '-');
      printf "\nCompiled successfully: %s\n%!" exe_path;
      printf "%s\n%!" (String.make 30 '-');
      exe_path
    )
    | _ -> (
      eprintf "Error: Failed to parse miva.toml.\n%!";
      exit 1;
    )
  )
let build_new_cmd = 
  let doc = "Compile the Miva project to an executable or a library." in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(
    const (fun verbose () ->
      let _res = build_project ~verbose in
      ()
    )
    $ verbose
    $ const ()
  )
let run_new_cmd = 
  let doc = "Compile and Run the Miva project." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(
    const (fun verbose () ->
      let exe_path = build_project ~verbose in
      let run_status = Sys.command exe_path in
      if run_status <> 0 then (
        eprintf "Error: Failed to run the executable.\n";
        eprintf "Exited with status %d.\n%!" run_status;
        eprintf "You can run it by ./%s\n%!" exe_path;
        exit 1;
      )
    )
    $ verbose
    $ const ()
  )
(* ---------- 子命令: clean ---------- *)
let clean_cmd = 
  let doc = "Clean the build artifacts in a Miva project." in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info Term.(
    const (fun _verbose () ->
      if Sys.file_exists "miva.toml" then (
        Sys.command "rm -rf build/" |> ignore;
        printf "Cleaned build artifacts.\n%!"
      ) else (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      )
    )
    $ verbose
    $ const ()
  )
(* ---------- 子命令: sin-build ---------- *)
let build_input_file =
  let doc = "Miva source file to compile" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let build_output_file =
  let doc = "Output executable name (default: <input> without extension)" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)

let build_cmd =
  let doc = "Compile an Miva source file to an executable." in
  let info = Cmd.info "sin-build" ~doc in
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

(* ---------- 子命令: sin-run ---------- *)
let run_input_file =
  let doc = "Miva source file to run" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let run_cmd =
  let doc = "Compile and run an Miva program." in
  let info = Cmd.info "sin-run" ~doc in
  Cmd.v info Term.(
    const (fun verbose input () ->
        let output = Filename.basename (Filename.remove_extension input) in
        let exe_path = compile_program ~verbose ~input_file:input ~output_file:output in
        if verbose then eprintf "Running %s...\n%!" exe_path;
        let run_status = Sys.command exe_path in
        (* 清理可执行文件，除非设置了保留标志 *)
        if not (try Sys.getenv "MIVA_KEEP_EXE" <> "" with Not_found -> false) then
          ignore (Sys.command (sprintf "rm -f %s" exe_path));
        exit run_status
      )
    $ verbose
    $ run_input_file
    $ const ()
  )

(* ---------- 子命令: test ---------- *)
let test_cmd = 
  let doc = "Run all tests in the Miva project." in
  let info = Cmd.info "test" ~doc in
  Cmd.v info Term.(
    const (fun verbose () ->
      if not (Sys.file_exists "miva.toml") then (
        eprintf "Error: Project not initialized in this directory.\n%!";
        exit 1;
      ) else (
        let _res = build_project ~verbose in
        let files = list_files (get_cache_dir () ^ "/src") in 
        let tests = List.filter (fun f -> String.ends_with ~suffix:".test.cpp" f) files in 
        List.iter (fun f -> (
          eprintf "Running %s...\n%!" f;
          let f = (get_cache_dir () ^ "/src/" ^ f) in
          let l = (String.length (get_cache_dir ())) in
          let mvpf = (
            String.sub f (l+1) (String.length f - l - 10)) ^ ".miva" in
          let ast = parse_input_file mvpf in
          let symb = SymbolTable.build_symbol_table ast in
          let res = List.map (fun s -> (
            let stdp = get_std_include_dir () in
            let objq = if String.starts_with ~prefix:stdp s then (
              (get_cache_dir ()) ^ "/" ^ Filename.remove_extension (remove_prefix stdp s)
            ) else (
              (get_cache_dir ()) ^ "/" ^ (Filename.remove_extension s)
            ) in
            objq ^ ".o"
          )) symb.files in
          let cache_dir = get_cache_dir () in 
          let std_include = get_std_include_dir () in
          let build_dir = get_build_dir () in
          let incf = get_include_flag () in
          let cmd =
            sprintf "g++ -O2 %s -o %s -I%s -I%s -I%s -std=c++20 %s -c %s"
              f ((Filename.remove_extension f) ^ ".o") cache_dir std_include build_dir 
              (if verbose then "" else "2>/dev/null") incf
          in
          let res = res @ [
            (Filename.remove_extension f) ^ ".o"; 
            (Filename.remove_extension (Filename.remove_extension f)) ^ ".o";
          ] in
          if Sys.command cmd <> 0 then (
            eprintf "Error compiling %s\n" f;
            eprintf "Running %s\n%!" cmd;
            exit 1;
          );
          let cmdn = sprintf "g++ %s -o %s %s %s" 
            (String.concat " " res) (Filename.remove_extension f) 
            (if verbose then "" else "2>/dev/null") (get_link_flag ()) in
          if Sys.command cmdn <> 0 then (
            eprintf "Error linking %s\nRunning %s\n%!" f cmdn;
            exit 1;
          );
          let _ = Sys.command (sprintf "./%s" (Filename.remove_extension f)) in
          ()
        )) tests
      )
    )
    $ verbose
    $ const ()
  )

(* ---------- 主命令 ---------- *)
let default_cmd =
  let doc = "The Miva programming language compiler" in
  let man = [
    `S "DESCRIPTION";
    `P "Miva is a systems programming language focused on explicitness, safety, and predictability.";
  ] in
  let info = Cmd.info "miva" ~version:Global.version ~doc ~man in
  Cmd.group info [
    build_cmd; 
    run_cmd; 
    init_cmd; 
    build_new_cmd; 
    run_new_cmd;
    clean_cmd;
    test_cmd;
  ]

let () =
  exit @@ Cmd.eval default_cmd